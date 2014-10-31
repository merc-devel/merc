from merc import feature
from merc import message


class LUsersFeature(feature.Feature):
  NAME = __name__


install = LUsersFeature.install


class LUserUnknown(message.Reply):
  NAME = "253"
  FORCE_TRAILING = True

  def __init__(self, num_unknown, reason="unknown connections", *args):
    self.num_unknown = num_unknown
    self.reason = reason

  def as_reply_params(self):
    return [self.num_unknown, self.reason]


class LUserChannels(message.Reply):
  NAME = "254"
  FORCE_TRAILING = True

  def __init__(self, num_channels, reason="channels formed", *args):
    self.num_channels = num_channels
    self.reason = reason

  def as_reply_params(self):
    return [self.num_channels, self.reason]


class LUserMe(message.Reply):
  NAME = "255"
  FORCE_TRAILING = True

  def __init__(self, reason, *args):
    self.reason = reason

  def as_reply_params(self):
    return [self.reason]


@LUsersFeature.register_user_command
class LUsers(message.Command):
  NAME = "LUSERS"
  MIN_ARITY = 0

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    app.run_hooks("server.luser.user", user)
    app.run_hooks("server.luser.oper", user)
    user.send_reply(LUserUnknown(str(0)))
    user.send_reply(LUserChannels(str(app.channels.count())))
    user.send_reply(LUserMe("I have {} clients and {} servers".format(
        app.users.count(), app.network.count())))


@LUsersFeature.hook("user.welcome")
def send_lusers_on_welcome(app, user):
  user.on_message(app, user.hostmask, LUsers())
