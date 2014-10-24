import collections
import datetime

from merc import feature
from merc import message
from merc import mode
from merc import util


Topic = collections.namedtuple("Topic", ["text", "who", "time"])
MAX_TOPIC_LENGTH = 390


class TopicFeature(feature.Feature):
  ISUPPORT = {
      "TOPICLEN": MAX_TOPIC_LENGTH
  }


install = TopicFeature


class NoTopic(message.Reply):
  NAME = "331"
  FORCE_TRAILING = True

  def __init__(self, channel_name):
    self.channel_name = channel_name

  def as_reply_params(self, client):
    return [self.channel_name, "No topic set"]


class TopicReply(message.Reply):
  NAME = "332"
  FORCE_TRAILING = True

  def __init__(self, channel_name, text):
    self.channel_name = channel_name
    self.text = text

  def as_reply_params(self, client):
    return [self.channel_name, self.text]


class TopicWhoTime(message.Reply):
  NAME = "333"

  def __init__(self, channel_name, who, time):
    self.channel_name = channel_name
    self.who = who
    self.time = time

  def as_reply_params(self, client):
    return [self.channel_name, self.who, str(int(self.time.timestamp()))]


@TopicFeature.register_command
class Topic(message.Command):
  NAME = "TOPIC"
  MIN_ARITY = 1

  def __init__(self, channel_name, text=None, *args):
    self.channel_name = channel_name
    self.text = text

  @property
  def FORCE_TRAILING(self):
    return self.text is not None

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    channel = client.server.get_channel(self.channel_name)

    if self.text is None:
      if channel.topic is not None:
        client.send_reply(TopicReply(channel.name, channel.topic.text))
        client.send_reply(TopicWhoTime(channel.name, channel.topic.who,
                                       channel.topic.time))
      else:
        client.send_reply(NoTopic(channel.name))
    else:
      if channel.is_topic_locked:
        channel.check_is_operator(client)

      if not self.text:
        channel.topic = None
      else:
        channel.topic = Topic(text[:MAX_TOPIC_LENGTH], client.hostmask,
                              datetime.datetime.utcnow())

      channel.broadcast(None, client.hostmask,
                        Topic(channel.name, channel.topic.text))

  def as_params(self, client):
    params = [self.channel_name]
    if self.text is not None:
      params.append(self.text)
    return params


@TopicFeature.hook("after_new_channel")
def set_initial_topic(channel):
  channel.topic = None


@TopicFeature.hook("after_channel_join")
def send_names_on_join(client, user, channel):
  if channel.topic is not None:
    user.on_message(user.hostmask, Topic(channel.name))


@TopicFeature.register_channel_mode
class TopicLock(mode.FlagMode):
  CHAR = "t"
  DEFAULT = True
