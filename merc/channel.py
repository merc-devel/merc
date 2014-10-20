import collections
import datetime
import re

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
  CHANNEL_REGEX = re.compile("^#[^, ]*$")

  MODES_WITHOUT_PARAMS = set("s")
  MODES_WITH_PARAMS = set(ChannelUser.ROLE_MODES.values())

  MAX_TOPIC_LENGTH = 390

  def __init__(self, name):
    if self.CHANNEL_REGEX.match(name) is None:
      raise errors.NoSuchChannel(name)

    self.name = name
    self.topic = None

    self.users = {}

  @property
  def normalized_name(self):
    return util.to_irc_lower(self.name)

  def broadcast(self, client, prefix, message):
    for user in self.users.values():
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
