from merc import feature
from merc import message


class LUsersFeature(feature.Feature):
  NAME = __name__


install = LUsersFeature.install


class LUserUnknown(message.Reply):
  NAME = "253"
  FORCE_TRAILING = True

  def as_reply_params(self, user):
    return ["0", "unknown connections"]


class LUserChannels(message.Reply):
  NAME = "254"
  FORCE_TRAILING = True

  def as_reply_params(self, user):
    return [str(len(user.server.channels)), "channels formed"]


class LUserMe(message.Reply):
  NAME = "255"
  FORCE_TRAILING = True

  def as_reply_params(self, user):
    return ["I have {} clients and {} servers".format(
        len(user.server.users),
        1)]


@LUsersFeature.register_command
class LUsers(message.Command):
  NAME = "LUSERS"
  MIN_ARITY = 0

  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    user.server.run_hooks("luser_user", user)
    user.server.run_hooks("luser_oper", user)
    user.send_reply(LUserUnknown())
    user.send_reply(LUserChannels())
    user.send_reply(LUserMe())


@LUsersFeature.hook("after_welcome")
def send_lusers_on_welcome(user):
  user.on_message(user.hostmask, LUsers())
