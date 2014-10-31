from merc import feature
from merc import message
from merc import mode


class ListFeature(feature.Feature):
  NAME = __name__


install = ListFeature.install


MAX_TARGETS = 1


@ListFeature.register_server_command
class ListStart(message.Reply):
  NAME = "321"
  FORCE_TRAILING = True
  MIN_ARITY = 2

  def __init__(self, heading="Channels", subheading="Users Name", *args):
    self.heading = heading
    self.subheading = subheading

  def as_reply_params(self):
    return [self.heading, self.subheading]


@ListFeature.register_server_command
class ListReply(message.Reply):
  NAME = "322"
  FORCE_TRAILING = True
  MIN_ARITY = 3

  def __init__(self, channel_name, num_visible, topic, *args):
    self.channel_name = channel_name
    self.num_visible = num_visible
    self.topic = topic

  def as_reply_params(self):
    return [self.channel_name, self.num_visible, self.topic]


@ListFeature.register_server_command
class ListEnd(message.Reply):
  NAME = "323"
  FORCE_TRAILING = True
  MIN_ARITY = 1

  def __init__(self, reason="End of /LIST", *args):
    self.reason = reason

  def as_reply_params(self):
    return [self.reason]


@ListFeature.register_user_command
class List(message.Command):
  NAME = "LIST"
  MIN_ARITY = 0

  def __init__(self, target=None, *args):
    self.target = target

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    user.send_reply(ListStart())

    try:
      if self.target is not None:
        channels = app.channels.query(self.target)
      else:
        channels = app.channels.all()

      for channel in channels:
        if not user.can_see_channel(channel):
          continue

        reply = ListReply(channel.name, str(len(channel.users)), "")
        app.run_hooks("server.list.modify", channel, reply)

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


@ListFeature.hook("server.targmax.modify")
def modify_targmax(app, targmax):
  targmax["LIST"] = MAX_TARGETS
