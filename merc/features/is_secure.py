from merc import feature
from merc import message
from merc import mode


class IsSecureFeature(feature.Feature):
  NAME = __name__


install = IsSecureFeature


@IsSecureFeature.register_user_mode
class SecurelyConnected(mode.FlagMode):
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

  def as_reply_params(self, client):
    return [self.nick, self.type, "is using a secure connection"]


@IsSecureFeature.hook("after_user_whois")
def send_whois_secure_if_secure(client, user):
  if user.is_securely_connected:
    client.send_reply(WhoIsSecure(user.nickname, "*"))
