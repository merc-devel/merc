import merc

from merc import errors
from merc import feature
from merc import message
from merc import util


class WelcomeFeature(feature.Feature):
  NAME = __name__


install = WelcomeFeature


class Welcome(message.Reply):
  NAME = "001"
  FORCE_TRAILING = True

  def as_reply_params(self, user):
    return ["Welcome to the {} Internet Relay Chat Network, {}".format(
        user.server.network_name,
        user.displayed_nickname)]


class YourHost(message.Reply):
  NAME = "002"
  FORCE_TRAILING = True

  def as_reply_params(self, user):
    return ["Your host is {}, running {}-{}".format(
        user.server.name,
        merc.__name__,
        merc.__version__)]


class Created(message.Reply):
  NAME = "003"
  FORCE_TRAILING = True

  def as_reply_params(self, user):
    return ["This server was created {}".format(
        user.server.creation_time.isoformat())]


class MyInfo(message.Reply):
  NAME = "004"

  def as_reply_params(self, user):
    return [user.server.name,
            "{}-{}".format(merc.__name__, merc.__version__),
            "".join(list(sorted(mode.CHAR
                                for mode in user.server.user_modes.values()))),
            "".join(list(sorted(mode.CHAR
                                for mode in user.server.channel_modes.values()
                                if not mode.TAKES_PARAM))),
            "".join(list(sorted(mode.CHAR
                                for mode in user.server.channel_modes.values()
                                if mode.TAKES_PARAM)))]


class ISupport(message.Reply):
  NAME = "005"
  FORCE_TRAILING = True

  def __init__(self, support_params):
    self.support_params = support_params

  def as_reply_params(self, user):
    return ["{}={}".format(k, v) for k, v in self.support_params.items()] + \
        ["are supported by this server"]


@WelcomeFeature.register_command
class User(message.Command):
  NAME = "USER"
  MIN_ARITY = 4

  def __init__(self, username, hostname, servername, realname, *args):
    self.username = username
    self.hostname = hostname
    self.servername = servername
    self.realname = realname

  def as_params(self, user):
    return [self.username, self.hostname, self.servername, self.realname]

  def handle_for(self, user, prefix):
    user.username = self.username
    user.realname = self.realname

    if user.is_ready_for_registration:
      user.server.register_user(user)


@WelcomeFeature.register_command
class Quit(message.Command):
  NAME = "QUIT"
  MIN_ARITY = 0

  def __init__(self, reason=None, *args):
    self.reason = reason

  @property
  def FORCE_TRAILING(self):
    return self.reason is not None

  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    user.close("Quit: " + self.reason if self.reason is not None
                                        else "Remote host closed connection")

  def as_params(self, user):
    params = []
    if self.reason is not None:
      params.append(self.reason)
    return params


@WelcomeFeature.hook("after_register")
def welcome_on_register(user):
  user.send_reply(Welcome())
  user.send_reply(YourHost())
  user.send_reply(Created())
  user.send_reply(MyInfo())
  user.send_reply(ISupport(user.server.isupport))

  user.server.run_hooks("after_welcome", user)


@WelcomeFeature.hook("before_remove_user")
def broadcast_quit_on_quit(user):
  user.relay_to_all(Quit(user.disconnect_reason))
