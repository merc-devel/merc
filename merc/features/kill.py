from merc import errors
from merc import message


@message.Command.register
class Kill(message.Command):
  NAME = "KILL"
  MIN_ARITY = 2

  def __init__(self, nickname, reason):
    self.nickname = nickname
    self.reason = reason

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    client.check_is_irc_operator()
    user = client.server.get_client(self.nickname)

    disconnect_reason = "Killed by {}: {}".format(client.nickname, self.reason)

    user.send(None, errors.Error(disconnect_reason))
    user.close(disconnect_reason)
