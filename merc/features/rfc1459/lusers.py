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


@LUsersFeature.register_command
class LUsers(message.Command):
  NAME = "LUSERS"
  MIN_ARITY = 0

  @message.Command.requires_registration
  def handle_for(self, server, user, prefix):
    server.run_hooks("luser_user", user)
    server.run_hooks("luser_oper", user)
    user.send_reply(LUserUnknown())
    user.send_reply(LUserChannels(server.channels.count()))
    user.send_reply(LUserMe(server.users.count(), 1))


@LUsersFeature.hook("after_welcome")
def send_lusers_on_welcome(server, user):
  user.on_message(server, user.hostmask, LUsers())
