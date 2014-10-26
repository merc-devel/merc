import collections
import datetime
import functools
import regex

from merc import errors
from merc import util


class ChannelUser(object):
  def __init__(self, channel, user):
    self.channel = channel
    self.user = user

    self.is_voiced = False
    self.is_halfop = False
    self.is_operator = False
    self.is_admin = False
    self.is_owner = False

  @property
  def sigil(self):
    # The first sigil is the highest, and thus the one we want to display.
    return self.sigils[:1]

  @property
  def sigils(self):
    sigils = ""

    if self.is_owner:
      sigils += "~"

    if self.is_admin:
      sigils += "&"

    if self.is_operator:
      sigils += "@"

    if self.is_halfop:
      sigils += "%"

    if self.is_voiced:
      sigils += "+"

    return sigils

  @property
  def is_voiced_equivalent(self):
    return self.is_voiced or self.is_halfop_equivalent

  @property
  def is_halfop_equivalent(self):
    return self.is_halfop or self.is_operator_equivalent

  @property
  def is_operator_equivalent(self):
    return self.is_operator or self.is_admin

  @property
  def is_admin_equivalent(self):
    return self.is_admin or self.is_owner_equivalent

  @property
  def is_owner_equivalent(self):
    return self.is_owner


class Channel(object):
  CHANNEL_NAME_REGEX = regex.compile(r"^[^\x00\x07\r\n,: ]*$")
  CHANNEL_CHARS = {"#", "&"}

  @classmethod
  def is_valid_name(cls, name):
    sigil, *rest = name

    return cls.CHANNEL_NAME_REGEX.match("".join(rest)) is not None and \
           sigil in cls.CHANNEL_CHARS

  def __init__(self, server, name):
    self.server = server

    if not self.is_valid_name(name):
      raise errors.NoSuchChannel(name)

    self.name = name
    self.creation_time = datetime.datetime.now()

    self.is_secret = True
    self.is_disallowing_external_messages = True
    self.is_moderated = False

    self.users = {}

    self.modes = {}
    self.feature_locals = {}

  @property
  def is_local(self):
    return self.name[0] == "&"

  def get_channel_user_for(self, user):
    try:
      return self.users[user.id]
    except KeyError:
      raise errors.NoSuchNick(self.name)

  @property
  def normalized_name(self):
    return util.to_irc_lower(self.name)

  def broadcast(self, user, prefix, message):
    for channel_user in list(self.users.values()):
      if channel_user.user is not user:
        channel_user.user.send(prefix, message)

  def join(self, user, key=None):
    channel_user = ChannelUser(self, user)

    if not self.users:
      channel_user.is_operator = True

    self.users[user.id] = channel_user
    user.channels[self.normalized_name] = self

  def part(self, user):
    del self.users[user.id]
    del user.channels[self.normalized_name]

  def has_user(self, user):
    return user.id in self.users

  def get_visible_users_for(self, user):
    if user.is_in_channel(self):
      yield from self.users.values()

    for channel_user in self.users.values():
      if not channel_user.user.is_invisible:
        yield channel_user

  def check_is_owner(self, user):
    try:
      channel_user = self.get_channel_user_for(user)
    except errors.NoSuchNick:
      raise errors.ChanOpPrivsNeeded(self.name)

    if not channel_user.is_owner_equivalent:
      raise errors.ChanOpPrivsNeeded(self.name)

  def check_is_admin(self, user):
    try:
      channel_user = self.get_channel_user_for(user)
    except errors.NoSuchNick:
      raise errors.ChanOpPrivsNeeded(self.name)

    if not channel_user.is_admin_equivalent:
      raise errors.ChanOpPrivsNeeded(self.name)

  def check_is_operator(self, user):
    try:
      channel_user = self.get_channel_user_for(user)
    except errors.NoSuchNick:
      raise errors.ChanOpPrivsNeeded(self.name)

    if not channel_user.is_operator_equivalent:
      raise errors.ChanOpPrivsNeeded(self.name)

  def check_is_halfop(self, user):
    try:
      channel_user = self.get_channel_user_for(user)
    except errors.NoSuchNick:
      raise errors.ChanOpPrivsNeeded(self.name)

    if not channel_user.is_halfop_equivalent:
      raise errors.ChanOpPrivsNeeded(self.name)

  def check_is_voiced(self, user):
    try:
      channel_user = self.get_channel_user_for(user)
    except errors.NoSuchNick:
      raise errors.CannotSendToChan(self.name)

    if not channel_user.is_voiced_equivalent:
      raise errors.CannotSendToChan(self.name)

  def check_has_user(self, user):
    if not self.has_user(user):
      raise errors.NoSuchNick(user.nickname)

  def get_feature_locals(self, feature):
    return self.feature_locals.setdefault(feature.NAME, {})
