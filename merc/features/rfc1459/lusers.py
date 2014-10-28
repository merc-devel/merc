from merc import feature
from merc import message


class LUsersFeature(feature.Feature):
  NAME = __name__


install = LUsersFeature.install


class LUserUnknown(message.Reply):
  NAME = "253"
  FORCE_TRAILING = True

  def as_reply_params(self):
    return ["0", "unknown connections"]


class LUserChannels(message.Reply):
  NAME = "254"
  FORCE_TRAILING = True

  def __init__(self, num_channels):
    self.num_channels = num_channels

  def as_reply_params(self):
    return [str(self.num_channels), "channels formed"]


class LUserMe(message.Reply):
  NAME = "255"
  FORCE_TRAILING = True

  def __init__(self, num_users, num_servers):
    self.num_users = num_users
    self.num_servers = num_servers

  def as_reply_params(self):
    return ["I have {} clients and {} servers".format(
        self.num_users, self.num_servers)]


@LUsersFeature.register_user_command
class LUsers(message.Command):
  NAME = "LUSERS"
  MIN_ARITY = 0

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    app.run_hooks("server.luser.user", user)
    app.run_hooks("server.luser.oper", user)
    user.send_reply(LUserUnknown())
    user.send_reply(LUserChannels(app.channels.count()))
    user.send_reply(LUserMe(app.users.count(), app.network.count()))


@LUsersFeature.hook("user.welcome")
def send_lusers_on_welcome(app, user):
  user.on_message(app, user.hostmask, LUsers())
