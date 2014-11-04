from merc import errors
from merc import feature
from merc import message


class KillFeature(feature.Feature):
  NAME = __name__


install = KillFeature.install


@KillFeature.register_user_command
class Kill(message.Command):
  NAME = "KILL"
  MIN_ARITY = 2

  def __init__(self, nickname, reason):
    self.nickname = nickname
    self.reason = reason

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    user.check_is_irc_operator()
    target = app.users.get(self.nickname)

    disconnect_reason = "Killed by {}: {}".format(user.nickname, self.reason)

    target.send(None, errors.Error(disconnect_reason))
    target.close(disconnect_reason)
