from merc import errors
from merc import feature
from merc import message
from merc import mode


class IsSecureFeature(feature.Feature):
  NAME = __name__


install = IsSecureFeature.install


class SecureOnlyChannel(errors.ParametrizedError):
  NAME = "489"
  REASON = "Channel can only be joined by securely connected users"


@IsSecureFeature.register_user_mode
class SecurelyConnected(mode.FlagMode, mode.UModeMixin):
  CHAR = "Z"

  def toggle(self):
    return False

  def get(self):
    return self.target.is_securely_connected


class WhoIsSecure(message.Reply):
  NAME = "671"
  FORCE_TRAILING = True

  def __init__(self, nick, type):
    self.nick = nick
    self.type = type

  def as_reply_params(self):
    return [self.nick, self.type, "is using a secure connection"]


@IsSecureFeature.hook("after_user_whois")
def send_whois_secure_if_secure(app, user, target):
  if target.is_securely_connected:
    user.send_reply(WhoIsSecure(target.nickname, "*"))


@IsSecureFeature.register_channel_mode
class SecureOnly(mode.FlagMode, mode.ChanModeMixin):
  CHAR = "S"


@IsSecureFeature.hook("check_join_channel")
def check_channel_ban(app, target, channel, key):
  if SecureOnly(channel).get() and not target.is_securely_connected:
    raise SecureOnlyChannel(channel.name)
