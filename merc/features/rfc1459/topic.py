import collections
import datetime

from merc import channel
from merc import feature
from merc import message
from merc import mode
from merc import util


MAX_TOPIC_LENGTH = 390


TopicDetail = collections.namedtuple("TopicDetail", ["text", "who", "time"])


class TopicFeature(feature.Feature):
  NAME = __name__


install = TopicFeature.install


class NoTopic(message.Reply):
  NAME = "331"
  FORCE_TRAILING = True

  def __init__(self, channel_name):
    self.channel_name = channel_name

  def as_reply_params(self, user):
    return [self.channel_name, "No topic set"]


class TopicReply(message.Reply):
  NAME = "332"
  FORCE_TRAILING = True

  def __init__(self, channel_name, text):
    self.channel_name = channel_name
    self.text = text

  def as_reply_params(self, user):
    return [self.channel_name, self.text]


class TopicWhoTime(message.Reply):
  NAME = "333"

  def __init__(self, channel_name, who, time):
    self.channel_name = channel_name
    self.who = who
    self.time = time

  def as_reply_params(self, user):
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
  def handle_for(self, user, prefix):
    chan = user.server.get_channel(self.channel_name)
    locals = chan.get_feature_locals(TopicFeature)

    current_topic = locals.get("topic", None)

    if self.text is None:
      if current_topic is not None:
        user.send_reply(TopicReply(chan.name, current_topic.text))
        user.send_reply(TopicWhoTime(chan.name, current_topic.who,
                                       current_topic.time))
      else:
        user.send_reply(NoTopic(chan.name))
    else:
      if TopicLock(chan).get():
        chan.check_is_operator(user)

      if not self.text:
        locals["topic"] = None
      else:
        locals["topic"] = TopicDetail(
            self.text[:MAX_TOPIC_LENGTH], user.hostmask,
            datetime.datetime.utcnow())

      chan.broadcast(
          None, user.hostmask,
          Topic(chan.name,
                locals["topic"].text if locals["topic"] is not None else ""))

  def as_params(self, user):
    params = [self.channel_name]
    if self.text is not None:
      params.append(self.text)
    return params


@TopicFeature.hook("after_join_channel")
def send_topic_on_join(user, target, channel):
  locals = channel.get_feature_locals(TopicFeature)
  current_topic = locals.get("topic", None)

  if current_topic is not None:
    target.on_message(target.hostmask, Topic(channel.name))


@TopicFeature.register_channel_mode
class TopicLock(mode.FlagMode, mode.ChanModeMixin):
  CHAR = "t"
  DEFAULT = True


@TopicFeature.hook("modify_list_reply")
def modify_list_reply(channel, reply):
  locals = channel.get_feature_locals(TopicFeature)

  current_topic = locals.get("topic", None)

  if current_topic is not None:
    reply.topic = current_topic.text


@TopicFeature.hook("modify_isupport")
def modify_isupport(server, isupport):
  isupport["TOPICLEN"] = MAX_TOPIC_LENGTH
