from merc import message


class BaseError(Exception, message.Message):
  pass


class Error(BaseError):
  NAME = "ERROR"
  FORCE_TRAILING = True

  def __init__(self, reason):
    self.reason = reason

  def as_params(self, client):
    return [self.reason]


class SimpleError(BaseError):
  def as_params(self, client):
    return [client.displayed_nickname, self.REASON]


class ParametrizedError(BaseError):
  def __init__(self, param):
    self.param = param

  def as_params(self, client):
    return [client.displayed_nickname, self.param, self.REASON]


class NoSuchNick(ParametrizedError):
  NAME = "401"
  REASON = "No such nick/channel"


class NoSuchChannel(ParametrizedError):
  NAME = "403"
  REASON = "No such channel"


class CannotSendToChan(ParametrizedError):
  NAME = "404"
  REASON = "Cannot send to channel"


class ErroneousNickname(SimpleError):
  NAME = "432"
  REASON = "Erroneous nickname"


class NicknameInUse(ParametrizedError):
  NAME = "433"
  REASON = "Nickname in use"


class NotRegistered(SimpleError):
  NAME = "451"
  REASON = "You have not registered"


class NeedMoreParams(ParametrizedError):
  NAME = "461"
  REASON = "Not enough parameters"


class UnknownCommand(ParametrizedError):
  NAME = "421"
  REASON = "Unknown command"


class UnknownMode(ParametrizedError):
  NAME = "472"
  REASON = "is an unknown mode char to me"


class UmodeUnknownFlag(SimpleError):
  NAME = "501"
  REASON = "Unknown MODE flag"


class UsersDontMatch(SimpleError):
  NAME = "502"
  REASON = "Can't change mode for other users"


class ChanOpPrivsNeeded(ParametrizedError):
  NAME = "482"
  REASON = "You're not a channel operator"
