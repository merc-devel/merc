import collections
import datetime
import functools
import regex

from merc import errors
from merc import util


class ChannelUser(object):
  def __init__(self, channel, client):
    self.channel = channel
    self.client = client

    self.is_voiced = False
    self.is_halfop = False
    self.is_operator = False
    self.is_admin = False
    self.is_owner = False

  @property
  def sigil(self):
    if self.is_owner:
      return "~"

    if self.is_admin:
      return "&"

    if self.is_operator:
      return "@"

    if self.is_halfop:
      return "%"

    if self.is_voiced:
      return "+"

    return ""


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

    self.users = {}

  @property
  def modes(self):
    modes = {}

    for feature in self.server.features.values():
      modes.update(feature.get_channel_modes(self))

    return modes

  @property
  def is_local(self):
    return self.name[0] == "&"

  def set_mode(self, client, mode, param=None):
    try:
      mode_obj = self.modes[mode]
    except KeyError:
      raise errors.UnknownMode(mode)

    return mode_obj.set(client, param)

  def unset_mode(self, client, mode, param=None):
    try:
      mode_obj = self.modes[mode]
    except KeyError:
      raise errors.UnknownMode(mode)

    return mode_obj.unset(client, param)

  def get_channel_user_for(self, client):
    try:
      return self.users[client.id]
    except KeyError:
      raise errors.NoSuchNick(self.name)

  @property
  def normalized_name(self):
    return util.to_irc_lower(self.name)

  def broadcast(self, client, prefix, message):
    for user in list(self.users.values()):
      if user.client is not client:
        user.client.send(prefix, message)

  def join(self, client, key=None):
    cu = ChannelUser(self, client)

    if not self.users:
      cu.is_operator = True

    self.users[client.id] = cu
    client.channels[self.normalized_name] = self

  def part(self, client):
    del self.users[client.id]
    del client.channels[self.normalized_name]

  def has_client(self, client):
    return client.id in self.users

  def get_visible_users_for(self, client):
    if client.is_in_channel(self):
      yield from self.users.values()

    for user in self.users.values():
      if not user.client.is_invisible:
        yield user

  def check_is_operator(self, client):
    try:
      channel_user = self.get_channel_user_for(client)
    except errors.NoSuchNick:
      raise errors.ChanOpPrivsNeeded(self.name)

    if not channel_user.is_operator and not channel_user.is_admin and \
       not channel_user.is_owner:
      raise errors.ChanOpPrivsNeeded(self.name)

  def check_is_voiced(self, client):
    try:
      channel_user = self.get_channel_user_for(client)
    except errors.NoSuchNick:
      raise errors.CannotSendToChan(self.name)

    if not channel_user.is_voiced and not channel_user.is_halfop:
      try:
        self.check_is_operator(client)
      except errors.ChanOpPrivsNeeded:
        raise errors.CannotSendToChan(self.name)

  def check_has_client(self, client):
    if not self.has_client(client):
      raise errors.NoSuchNick(client.nickname)

  def get_feature_locals(self, feature):
    return self.server.features[feature.NAME].get_channel_locals(self)
