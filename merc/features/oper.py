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

  def as_reply_params(self, user):
    return [str(sum(user.is_irc_operator
                    for user in user.server.users.values())),
            "IRC operators online"]


class YoureOper(message.Reply):
  NAME = "381"
  FORCE_TRAILING = True

  def as_reply_params(self, user):
    return ["You are now an IRC operator"]


@OperFeature.register_command
class Kill(message.Command):
  NAME = "KILL"
  MIN_ARITY = 2

  def __init__(self, nickname, reason):
    self.nickname = nickname
    self.reason = reason

  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    user.check_is_irc_operator()
    target = user.server.get_user(self.nickname)

    disconnect_reason = "Killed by {}: {}".format(user.nickname, self.reason)

    target.send(None, errors.Error(disconnect_reason))
    target.close(disconnect_reason)


@OperFeature.register_command
class Oper(message.Command):
  NAME = "OPER"
  MIN_ARITY = 2

  def __init__(self, username, password, *args):
    self.username = username
    self.password = password

  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    try:
      oper_spec = user.server.config["opers"][self.username]
    except KeyError:
      raise errors.PasswordMismatch

    if not any(user.hostmask_matches(hostmask)
               for hostmask in oper_spec["hostmasks"]):
      raise errors.PasswordMismatch

    if not user.server.crypt_context.verify(self.password,
                                              oper_spec["password"]):
      raise errors.PasswordMismatch

    user.is_irc_operator = True
    user.send_reply(YoureOper())
    user.server.run_hooks("user_mode_change", user, [(Operator, "+", None)])


@OperFeature.hook("luser_oper")
def show_luser_oper(user):
  user.send_reply(LUserOp())


@OperFeature.register_user_mode
class Operator(mode.Mode):
  CHAR = "o"
  TAKES_PARAM = False

  def set(self, user, param):
    return False

  def unset(self, user, param):
    if not self.target.is_irc_operator:
      return False

    self.target.is_irc_operator = False
    return True

  def get(self):
    return self.target.is_irc_operator
