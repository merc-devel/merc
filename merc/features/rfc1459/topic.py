import collections
import datetime

from merc import feature
from merc import message
from merc import mode


MAX_TOPIC_LENGTH = 390


TopicDetail = collections.namedtuple("TopicDetail", ["text", "who", "time"])


class TopicFeature(feature.Feature):
  NAME = __name__


install = TopicFeature.install


class NoTopic(message.Reply):
  NAME = "331"
  FORCE_TRAILING = True
  MIN_ARITY = 2

  def __init__(self, channel_name, reason="No topic set", *args):
    self.channel_name = channel_name
    self.reason = reason

  def as_reply_params(self):
    return [self.channel_name, self.reason]


class TopicReply(message.Reply):
  NAME = "332"
  FORCE_TRAILING = True
  MIN_ARITY = 2

  def __init__(self, channel_name, text, *args):
    self.channel_name = channel_name
    self.text = text

  def as_reply_params(self):
    return [self.channel_name, self.text]


class TopicWhoTime(message.Reply):
  NAME = "333"
  MIN_ARITY = 3

  def __init__(self, channel_name, who, time, *args):
    self.channel_name = channel_name
    self.who = who
    self.time = time

  def as_reply_params(self):
    return [self.channel_name, self.who, self.time]


@TopicFeature.register_user_command
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
  def handle_for(self, app, user, prefix):
    chan = app.channels.get(self.channel_name)
    locals = chan.get_feature_locals(TopicFeature)

    current_topic = locals.get("topic", None)

    if self.text is None:
      if current_topic is not None:
        user.send_reply(TopicReply(chan.name, current_topic.text))
        user.send_reply(TopicWhoTime(chan.name, current_topic.who,
                                     str(int(current_topic.time.timestamp()))))
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

  def as_command_params(self):
    params = [self.channel_name]
    if self.text is not None:
      params.append(self.text)
    return params


@TopicFeature.hook("channel.join")
def send_topic_on_join(app, user, target, channel):
  locals = channel.get_feature_locals(TopicFeature)
  current_topic = locals.get("topic", None)

  if current_topic is not None:
    target.on_message(app, target.hostmask, Topic(channel.name))


@TopicFeature.register_channel_mode
class TopicLock(mode.FlagMode, mode.ChanModeMixin):
  CHAR = "t"
  DEFAULT = True


@TopicFeature.hook("server.list.modify")
def modify_list_reply(app, channel, reply):
  locals = channel.get_feature_locals(TopicFeature)

  current_topic = locals.get("topic", None)

  if current_topic is not None:
    reply.topic = current_topic.text


@TopicFeature.hook("server.isupport.modify")
def modify_isupport(app, isupport):
  isupport["TOPICLEN"] = MAX_TOPIC_LENGTH
