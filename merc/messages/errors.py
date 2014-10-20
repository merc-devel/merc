from merc.messages import message


class ErrorMessage(Exception, message.Message):
  pass


class Error(ErrorMessage):
  NAME = "ERROR"

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


class NoSuchChannel(ParametrizedErrorMessage):
  NAME = "403"
  REASON = "No such channel"


class ErroneousNickname(SimpleErrorMessage):
  NAME = "432"
  REASON = "Erroneous nickname"


class NicknameInUse(ParametrizedErrorMessage):
  NAME = "433"
  REASON = "Nickname in use"


class NeedMoreParams(ParametrizedErrorMessage):
  NAME = "461"
  REASON = "Not enough parameters"


class UnknownCommand(ParametrizedErrorMessage):
  NAME = "421"
  REASON = "Unknown command"
