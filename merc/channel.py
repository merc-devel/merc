import collections
import datetime
import regex

from merc import util
from merc.messages import errors


Topic = collections.namedtuple("Topic", ["text", "who", "time"])


class ChannelUser(object):
  ROLE_NORMAL = 0
  ROLE_VOICED = 1
  ROLE_HALFOP = 2
  ROLE_OPERATOR = 3
  ROLE_ADMIN = 4
  ROLE_OWNER = 5

  ROLE_CHARS = {
    ROLE_VOICED: "+",
    ROLE_HALFOP: "%",
    ROLE_OPERATOR: "@",
    ROLE_ADMIN: "&",
    ROLE_OWNER: "~"
  }

  ROLE_MODES = {
    ROLE_VOICED: "v",
    ROLE_HALFOP: "h",
    ROLE_OPERATOR: "o",
    ROLE_ADMIN: "a",
    ROLE_OWNER: "q"
  }

  def __init__(self, client):
    self.client = client
    self.role = ChannelUser.ROLE_NORMAL


class Channel(object):
  CHANNEL_REGEX = regex.compile(r"^#[^\x00\x07\r\n,: ]*$")

  MODES_WITHOUT_PARAMS = set("ns")
  MODES_WITH_PARAMS = set(ChannelUser.ROLE_MODES.values())

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

  def set_mode(self, mode, param=None):
    try:
      set, _ = self.MODES[mode]
    except KeyError:
      raise errors.UnknownMode(mode)

    return set(self, param)

  def unset_mode(self, mode, param=None):
    try:
      _, unset = self.MODES[mode]
    except KeyError:
      raise errors.UnknownMode(mode)

    return unset(self, param)

  @property
  def normalized_name(self):
    return util.to_irc_lower(self.name)

  def broadcast(self, client, prefix, message):
    for user in list(self.users.values()):
      if user.client is not client:
        user.client.send(prefix, message)

  def join(self, client, key=None):
    self.users[client.id] = ChannelUser(client)
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

  def mutate_disallowing_external_messages(self, flag):
    self.is_disallowing_external_messages = flag
    return True

  def mutate_secret(self, flag):
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
    "s": util.make_flag_pair(mutate_secret)
  }
