import collections
import datetime
import fnmatch
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
    return self.is_operator or self.is_admin_equivalent

  @property
  def is_admin_equivalent(self):
    return self.is_admin or self.is_owner_equivalent

  @property
  def is_owner_equivalent(self):
    return self.is_owner


class Channel(object):
  CHANNEL_CHARS = {"#", "&"}

  @classmethod
  def is_channel_name(cls, name):
    sigil, *_ = name
    return sigil in cls.CHANNEL_CHARS

  def __init__(self, server, name):
    self.server = server
    self.name = name
    self.creation_time = datetime.datetime.now()

    self.is_secret = True
    self.is_disallowing_external_messages = True
    self.is_moderated = False

    self.users = {}

    self.modes = {}
    self.feature_locals = {}

  def name_matches(self, pattern):
    return regex.match(fnmatch.translate(util.to_irc_lower(pattern)),
                       util.to_irc_lower(self.name)) is not None

  @property
  def is_local(self):
    return self.name[0] == "&"

  def get_channel_user_for(self, user):
    try:
      return self.users[user.id]
    except KeyError:
      raise errors.NotOnChannel(self.name)

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

    if not self.users:
      self.server.channels.remove(self)

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


class ChannelStore(object):
  def __init__(self, server):
    self.server = server
    self.channels = {}

  def get(self, name):
    try:
      return self.channels[util.to_irc_lower(name)]
    except KeyError:
      raise errors.NoSuchNick(name)

  def has(self, name):
    return util.to_irc_lower(name) in self.channels

  def new(self, name):
    c = Channel(self.server, name)
    self.channels[c.normalized_name] = c
    return c

  def remove(self, channel):
    del self.channels[channel.normalized_name]
    self.server.run_hooks("after_remove_channel", channel)

  def query(self, pattern):
    return (channel for channel in self.channels.values()
                    if channel.name_matches(pattern))

  def all(self):
    return self.channels.values()

  def count(self):
    return len(self.channels)

  @property
  def modes(self):
    modes = {}

    for feature in self.server.features.values():
      modes.update(feature.CHANNEL_MODES)

    return modes
