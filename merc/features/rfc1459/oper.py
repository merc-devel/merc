from merc import errors
from merc import feature
from merc import message
from merc import mode


class OperFeature(feature.Feature):
  NAME = __name__
  CONFIG_SECTION = 'opers'


install = OperFeature.install


@OperFeature.register_server_command
class LUserOp(message.Reply):
  NAME = "252"
  FORCE_TRAILING = True
  MIN_ARITY = 2

  def __init__(self, num_irc_operators, reason="IRC operators online", *args):
    self.num_irc_operators = num_irc_operators
    self.reason = reason

  def as_reply_params(self):
    return [self.num_irc_operators, self.reason]


@OperFeature.register_server_command
class YoureOper(message.Reply):
  NAME = "381"
  FORCE_TRAILING = True
  MIN_ARITY = 1

  def __init__(self, reason="You are now an IRC operator", *args):
    self.reason = reason

  def as_reply_params(self):
    return [self.reason]


@OperFeature.register_server_command
class StatsOLine(message.Reply):
  NAME = "243"
  MIN_ARITY = 4

  def __init__(self, o, hostmask, star, name, *args):
    self.o = o
    self.hostmask = hostmask
    self.star = star
    self.name = name

  def as_reply_params(self):
    return [self.o, self.hostmask, self.star, self.name]


@OperFeature.register_user_command
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


@OperFeature.register_user_command
class Oper(message.Command):
  NAME = "OPER"
  MIN_ARITY = 2

  def __init__(self, username, password, *args):
    self.username = username
    self.password = password

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    try:
      oper_spec = app.config["opers"][self.username]
    except KeyError:
      raise errors.PasswordMismatch

    if not any(user.hostmask_matches(hostmask)
               for hostmask in oper_spec["hostmasks"]):
      raise errors.PasswordMismatch

    if not app.crypt_context.verify(self.password, oper_spec["password"]):
      raise errors.PasswordMismatch

    user.is_irc_operator = True
    user.send_reply(YoureOper())
    app.run_hooks("user.mode_change", user, [(Operator, "+", None)])


@OperFeature.hook("server.luser.oper")
def show_luser_oper(app, user):
  user.send_reply(LUserOp(str(sum(user.is_irc_operator
                                  for user in app.users.all()))))
@OperFeature.hook("server.stats.o")
def send_oper_hosts(app, user):
  try:
    user.check_is_irc_operator()
  except errors.BaseError as e:
    user.send_reply(e)
  else:
    for name, oper in app.config["opers"].items():
      for mask in oper["hostmasks"]:
        user.send_reply(StatsOLine("O", mask, "*", name))


@OperFeature.register_user_mode
class Operator(mode.Mode, mode.UModeMixin):
  CHAR = "o"
  TAKES_PARAM = False

  def set(self, app, user, param):
    return False

  def unset(self, app, user, param):
    if not self.target.is_irc_operator:
      return False

    self.target.is_irc_operator = False
    return True

  def get(self):
    return self.target.is_irc_operator
