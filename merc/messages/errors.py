from merc.messages import message


class ErrorMessage(Exception, message.Message):
  pass


class Error(ErrorMessage):
  NAME = "ERROR"
  FORCE_TRAILING = True

  def __init__(self, reason):
    self.reason = reason

  def as_params(self, client):
    return [self.reason]


class SimpleErrorMessage(ErrorMessage):
  def as_params(self, client):
    return [client.displayed_nickname, self.REASON]


class ParametrizedErrorMessage(ErrorMessage):
  def __init__(self, param):
    self.param = param

  def as_params(self, client):
    return [client.displayed_nickname, self.param, self.REASON]


class NoSuchNick(ParametrizedErrorMessage):
  NAME = "401"
  REASON = "No such nick/channel"


class NoSuchChannel(ParametrizedErrorMessage):
  NAME = "403"
  REASON = "No such channel"


class ErroneousNickname(SimpleErrorMessage):
  NAME = "432"
  REASON = "Erroneous nickname"


class NicknameInUse(ParametrizedErrorMessage):
  NAME = "433"
  REASON = "Nickname in use"


class NotRegistered(SimpleErrorMessage):
  NAME = "451"
  REASON = "You have not registered"


class NeedMoreParams(ParametrizedErrorMessage):
  NAME = "461"
  REASON = "Not enough parameters"


class UnknownCommand(ParametrizedErrorMessage):
  NAME = "421"
  REASON = "Unknown command"


class UnknownMode(ParametrizedErrorMessage):
  NAME = "472"
  REASON = "is an unknown mode char to me"


class UmodeUnknownFlag(SimpleErrorMessage):
  NAME = "501"
  REASON = "Unknown MODE flag"


class UsersDontMatch(SimpleErrorMessage):
  NAME = "502"
  REASON = "Can't change mode for other users"
