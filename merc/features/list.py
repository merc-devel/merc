from merc import feature
from merc import message
from merc import mode


class ListFeature(feature.Feature):
  NAME = __name__


install = ListFeature


class ListStart(message.Reply):
  NAME = "321"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["Channels", "Users Name"]


class ListReply(message.Reply):
  NAME = "322"
  FORCE_TRAILING = True

  def __init__(self, channel_name, num_visible, topic):
    self.channel_name = channel_name
    self.num_visible = num_visible
    self.topic = topic

  def as_reply_params(self, client):
    return [self.channel_name, str(self.num_visible), self.topic]


class ListEnd(message.Reply):
  NAME = "323"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["End of /LIST"]


@ListFeature.register_command
class List(message.Command):
  NAME = "LIST"
  MIN_ARITY = 0

  def __init__(self, target=None, *args):
    self.target = target

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    client.send_reply(ListStart())

    try:
      if self.target is not None:
        channels = [client.server.get_channel(self.target)]
      else:
        channels = (channel for channel in client.server.channels.values())

      for channel in channels:
        if not client.can_see_channel(channel):
          continue

        reply = ListReply(channel.name, len(channel.users), "")
        client.server.run_hooks("modify_list_reply", channel, reply)

        client.send_reply(reply)
    finally:
      client.send_reply(ListEnd())


@ListFeature.register_channel_mode
class Secret(mode.FlagMode):
  CHAR = "s"
  TAKES_PARAM = False

  def toggle(self):
    self.target.is_secret = not self.target.is_secret
    return True

  def get(self):
    return self.target.is_secret
