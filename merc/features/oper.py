from merc import errors
from merc import feature
from merc import message
from merc import mode


class OperFeature(feature.Feature):
  NAME = __name__


install = OperFeature


class LUserOp(message.Reply):
  NAME = "252"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return [str(sum(client.is_irc_operator
                    for client in client.server.clients.values())),
            "IRC operators online"]


class YoureOper(message.Reply):
  NAME = "381"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["You are now an IRC operator"]


@OperFeature.register_command
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


@OperFeature.register_command
class Oper(message.Command):
  NAME = "OPER"
  MIN_ARITY = 2

  def __init__(self, username, password, *args):
    self.username = username
    self.password = password

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    try:
      oper_spec = client.server.config["opers"][self.username]
    except KeyError:
      raise errors.PasswordMismatch

    if not any(client.hostmask_matches(hostmask)
               for hostmask in oper_spec["hostmasks"]):
      raise errors.PasswordMismatch

    if not client.server.crypt_context.verify(self.password,
                                              oper_spec["password"]):
      raise errors.PasswordMismatch

    client.is_irc_operator = True
    client.send_reply(YoureOper())
    client.server.run_hooks("user_mode_change", client, {
        Operator.CHAR: client.modes[Operator.CHAR]})


@OperFeature.hook("luser_oper")
def show_luser_oper(self, client):
  client.send_reply(LUserOp())


@OperFeature.register_user_mode
class Operator(mode.Mode):
  CHAR = "o"

  def set(self):
    return False

  def unset(self):
    if not self.target.is_irc_operator:
      return False

    self.target.is_irc_operator = False
    return True

  def get(self):
    return self.target.is_irc_operator
