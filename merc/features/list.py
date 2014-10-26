from merc import feature
from merc import message
from merc import mode


class ListFeature(feature.Feature):
  NAME = __name__


install = ListFeature


MAX_TARGETS = 1


class ListStart(message.Reply):
  NAME = "321"
  FORCE_TRAILING = True

  def as_reply_params(self, user):
    return ["Channels", "Users Name"]


class ListReply(message.Reply):
  NAME = "322"
  FORCE_TRAILING = True

  def __init__(self, channel_name, num_visible, topic):
    self.channel_name = channel_name
    self.num_visible = num_visible
    self.topic = topic

  def as_reply_params(self, user):
    return [self.channel_name, str(self.num_visible), self.topic]


class ListEnd(message.Reply):
  NAME = "323"
  FORCE_TRAILING = True

  def as_reply_params(self, user):
    return ["End of /LIST"]


@ListFeature.register_command
class List(message.Command):
  NAME = "LIST"
  MIN_ARITY = 0

  def __init__(self, target=None, *args):
    self.target = target

  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    user.send_reply(ListStart())

    try:
      if self.target is not None:
        channels = [user.server.get_channel(self.target)]
      else:
        channels = (channel for channel in user.server.channels.values())

      for channel in channels:
        if not user.can_see_channel(channel):
          continue

        reply = ListReply(channel.name, len(channel.users), "")
        user.server.run_hooks("modify_list_reply", channel, reply)

        user.send_reply(reply)
    finally:
      user.send_reply(ListEnd())


@ListFeature.register_channel_mode
class Secret(mode.FlagMode, mode.ChanModeMixin):
  CHAR = "s"
  TAKES_PARAM = False

  def toggle(self):
    self.target.is_secret = not self.target.is_secret
    return True

  def get(self):
    return self.target.is_secret


@ListFeature.hook("modify_targmax")
def modify_targmax(targmax):
  targmax["LIST"] = MAX_TARGETS
