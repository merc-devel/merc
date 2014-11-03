from merc import errors
from merc import feature
from merc import message
from merc import mode


class TLSSupportFeature(feature.Feature):
  NAME = __name__


install = TLSSupportFeature.install


class SecureOnlyChannel(errors.ParametrizedError):
  NAME = "489"
  REASON = "Channel can only be joined by securely connected users"


@TLSSupportFeature.register_user_mode
class SecurelyConnected(mode.FlagMode, mode.UModeMixin):
  CHAR = "Z"

  def toggle(self):
    return False

  def get(self):
    return self.target.is_securely_connected


@TLSSupportFeature.register_server_command
class WhoIsSecure(message.Reply):
  NAME = "671"
  FORCE_TRAILING = True
  MIN_ARITY = 3

  def __init__(self, nick, type, reason="is using a secure connection", *args):
    self.nick = nick
    self.type = type
    self.reason = reason

  def as_reply_params(self):
    return [self.nick, self.type, self.reason]


@TLSSupportFeature.hook("user.whois")
def send_whois_secure_if_secure(app, user, target):
  if target.is_securely_connected:
    user.send_reply(WhoIsSecure(target.nickname, "*"))


@TLSSupportFeature.register_channel_mode
class SecureOnly(mode.FlagMode, mode.ChanModeMixin):
  CHAR = "S"


@TLSSupportFeature.hook("channel.join.check")
def check_channel_ban(app, target, channel, key):
  if SecureOnly(channel).get() and not target.is_securely_connected:
    raise SecureOnlyChannel(channel.name)
