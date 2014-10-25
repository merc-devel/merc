import regex

import merc

from merc import errors
from merc import feature
from merc import message
from merc import util


MAX_NICKNAME_LENGTH = 12
NICKNAME_REGEX = regex.compile(r"^[\p{L}\p{So}_\[\]\\^{}|`][\p{L}\p{So}\p{N}_\[\]\\^{}|`-]*$")


class WelcomeFeature(feature.Feature):
  NAME = __name__
  ISUPPORT = {
      "NICKLEN": MAX_NICKNAME_LENGTH
  }


install = WelcomeFeature


class Welcome(message.Reply):
  NAME = "001"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["Welcome to the {} Internet Relay Chat Network, {}".format(
        client.server.network_name,
        client.displayed_nickname)]


class YourHost(message.Reply):
  NAME = "002"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["Your host is {}, running {}-{}".format(
        client.server.name,
        merc.__name__,
        merc.__version__)]


class Created(message.Reply):
  NAME = "003"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["This server was created {}".format(
        client.server.creation_time.isoformat())]


class MyInfo(message.Reply):
  NAME = "004"

  def as_reply_params(self, client):
    return [client.server.name,
            "{}-{}".format(merc.__name__, merc.__version__),
            "".join(list(sorted(mode.CHAR
                                for mode in client.server.user_modes.values()))),
            "".join(list(sorted(mode.CHAR
                                for mode in client.server.channel_modes.values()
                                if not mode.TAKES_PARAM))),
            "".join(list(sorted(mode.CHAR
                                for mode in client.server.channel_modes.values()
                                if mode.TAKES_PARAM)))]


class ISupport(message.Reply):
  NAME = "005"
  FORCE_TRAILING = True

  def __init__(self, support_params):
    self.support_params = support_params

  def as_reply_params(self, client):
    return ["{}={}".format(k, v) for k, v in self.support_params.items()] + \
        ["are supported by this server"]


@WelcomeFeature.register_command
class Nick(message.Command):
  NAME = "NICK"
  MIN_ARITY = 1

  def __init__(self, nickname, *args):
    self.nickname = nickname

  def as_params(self, client):
    return [self.nickname]

  def handle_for(self, client, prefix):
    old_hostmask = client.hostmask

    if NICKNAME_REGEX.match(self.nickname) is None or \
        len(self.nickname) > MAX_NICKNAME_LENGTH:
      raise errors.ErroneousNickname

    client.server.rename_client(client, self.nickname)

    if client.is_registered:
      client.relay_to_all(Nick(self.nickname), old_hostmask)
      client.send(old_hostmask, Nick(self.nickname))
    else:
      if client.is_ready_for_registration:
        client.register()


@WelcomeFeature.register_command
class User(message.Command):
  NAME = "USER"
  MIN_ARITY = 4

  def __init__(self, user, hostname, servername, realname, *args):
    self.user = user
    self.hostname = hostname
    self.servername = servername
    self.realname = realname

  def as_params(self, client):
    return [self.user, self.hostname, self.servername, self.realname]

  def handle_for(self, client, prefix):
    client.username = self.user
    client.realname = self.realname

    if client.is_ready_for_registration:
      client.register()


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
  def handle_for(self, client, prefix):
    client.close("Quit: " + self.reason if self.reason is not None
                                        else "Remote host closed connection")

  def as_params(self, client):
    params = []
    if self.reason is not None:
      params.append(self.reason)
    return params


@WelcomeFeature.hook("after_register")
def welcome_on_register(client):
  client.send_reply(Welcome())
  client.send_reply(YourHost())
  client.send_reply(Created())
  client.send_reply(MyInfo())
  client.send_reply(ISupport(client.server.isupport))

  client.server.run_hooks("after_welcome", client)


@WelcomeFeature.hook("before_remove_client")
def broadcast_quit_on_quit(client):
  client.relay_to_all(Quit(client.disconnect_reason))
