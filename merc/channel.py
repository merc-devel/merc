import collections
import datetime
import functools
import regex

from merc import util
from merc.messages import errors


Topic = collections.namedtuple("Topic", ["text", "who", "time"])



def chanmode_needs_operator(f):
  @functools.wraps(f)
  def _wrapper(self, client, value):
    try:
      channel_user = self.get_channel_user_for(client)
    except errors.NoSuchNick:
      raise errors.ChanOpPrivsNeeded(self.name)

    if not channel_user.is_operator:
      raise errors.ChanOpPrivsNeeded(self.name)

    return f(self, client, value)
  return _wrapper


def chanrole_needs_operator(f):
  @functools.wraps(f)
  def _wrapper(self, client, value):
    try:
      channel_user = self.channel.get_channel_user_for(client)
    except errors.NoSuchNick:
      raise errors.ChanOpPrivsNeeded(self.channel.name)

    if not channel_user.is_operator:
      raise errors.ChanOpPrivsNeeded(self.channel.name)

    return f(self, client, value)
  return _wrapper


class ChannelUser(object):
  ROLE_CHARS = "~&@%+"
  ROLE_MODES = "qaohv"

  def __init__(self, channel, client):
    self.channel = channel
    self.client = client

    self.is_voiced = False
    self.is_halfop = False
    self.is_operator = False
    self.is_admin = False
    self.is_owner = False

  @chanrole_needs_operator
  def mutate_operator(self, client, value):
    if self.is_operator == value:
      return False

    self.is_operator = value
    return True

  @staticmethod
  def make_role_setter_pair(mutator):
    def setter(channel, client, param):
      user = client.server.get_client(param)
      return mutator(channel.get_channel_user_for(user), client, True)

    def unsetter(channel, client, param):
      user = client.server.get_client(param)
      return mutator(channel.get_channel_user_for(user), client, False)

    return (setter, unsetter)

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
  CHANNEL_REGEX = regex.compile(r"^#[^\x00\x07\r\n,: ]*$")

  MAX_TOPIC_LENGTH = 390

  def __init__(self, name):
    if self.CHANNEL_REGEX.match(name) is None:
      raise errors.NoSuchChannel(name)

    self.name = name
    self.topic = None
    self.creation_time = datetime.datetime.utcnow()

    self.is_disallowing_external_messages = False
    self.is_secret = False

    self.users = {}

  def set_mode(self, client, mode, param=None):
    try:
      set, _ = self.MODES[mode]
    except KeyError:
      raise errors.UnknownMode(mode)

    return set(self, client, param)

  def unset_mode(self, client, mode, param=None):
    try:
      _, unset = self.MODES[mode]
    except KeyError:
      raise errors.UnknownMode(mode)

    return unset(self, client, param)

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

  def set_topic(self, client, text):
    if not text:
      self.topic = None
    else:
      self.topic = Topic(text[:self.MAX_TOPIC_LENGTH],
                         client.hostmask, datetime.datetime.utcnow())

  def has_client(self, client):
    return client.id in self.users

  def get_visible_users_for(self, client):
    if client.is_in_channel(self):
      yield from self.users.values()

    for user in self.users.values():
      if not user.client.is_invisible:
        yield user

  @chanmode_needs_operator
  def mutate_disallowing_external_messages(self, client, flag):
    if self.is_disallowing_external_messages == flag:
      return False

    self.is_disallowing_external_messages = flag
    return True

  @chanmode_needs_operator
  def mutate_secret(self, client, flag):
    if self.is_secret == flag:
      return False

    self.is_secret = flag
    return True

  @property
  def modes(self):
    modes = {}

    if self.is_secret:
      modes["s"] = True

    if self.is_disallowing_external_messages:
      modes["n"] = True

    return modes

  MODES = {
    "n": util.make_flag_pair(mutate_disallowing_external_messages),
    "s": util.make_flag_pair(mutate_secret),
    "o": ChannelUser.make_role_setter_pair(ChannelUser.mutate_operator)
  }

  MODES_WITHOUT_PARAMS = set("ns")
  MODES_WITH_PARAMS = set(ChannelUser.ROLE_MODES)
