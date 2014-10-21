from merc import message


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


@message.Command.register
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

      channel.set_topic(client, self.text)

      client.relay_to_channel(channel, Topic(channel.name, channel.topic.text))
      client.relay_to_self(Topic(channel.name, channel.topic.text))

  def as_params(self, client):
    params = [self.channel_name]
    if self.text is not None:
      params.append(self.text)
    return params
