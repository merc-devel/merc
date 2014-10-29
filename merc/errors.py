from merc import message


class BaseError(Exception, message.Reply):
  pass


class Error(Exception, message.Message):
  NAME = "ERROR"
  FORCE_TRAILING = True

  def __init__(self, reason):
    self.reason = reason

  def as_params(self, client):
    return [self.reason]


class LinkError(Error):
  NAME = "ERROR"
  FORCE_TRAILING = True

  def as_params(self, client):
    host, *_ = client.protocol.transport.get_extra_info("peername")
    return ["Closing link: {} ({})".format(host, self.reason)]


class SimpleError(BaseError):
  def as_reply_params(self):
    return [self.REASON]


class ParametrizedError(BaseError):
  def __init__(self, *params):
    self.params = params

  def as_reply_params(self):
    return list(self.params) + [self.REASON]


class NoSuchNick(ParametrizedError):
  NAME = "401"
  REASON = "No such nick/channel"


class NoSuchServer(ParametrizedError):
  NAME = "402"
  REASON = "No such server"


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


class NotOnChannel(ParametrizedError):
  NAME = "442"
  REASON = "You're not on that channel"


class AlreadyOnChannel(ParametrizedError):
  NAME = "443"
  REASON = "is already on channel"


class PasswordMismatch(SimpleError):
  NAME = "464"
  REASON = "Password mismatch"


class NoPrivileges(SimpleError):
  NAME = "481"
  REASON = "You're not an IRC operator"


class BannedFromChannel(ParametrizedError):
  NAME = "474"
  REASON = "You are banned from the channel"


class AlreadyRegistered(SimpleError):
  NAME = "462"
  REASON = "You may not reregister"
