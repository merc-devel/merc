import merc

from merc import message
from merc import util
from merc.features import lusers
from merc.features import mode
from merc.features import motd


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
    from merc import client as c
    from merc import channel

    return [client.server.name,
            "{}-{}".format(merc.__name__, merc.__version__),
            "".join(list(sorted(c.Client.MODES))),
            "".join(list(sorted(channel.Channel.MODES_WITHOUT_PARAMS))),
            "".join(list(sorted(channel.Channel.MODES_WITH_PARAMS)))]


class ISupport(message.Reply):
  NAME = "005"
  FORCE_TRAILING = True

  def __init__(self, support_params):
    self.support_params = support_params

  def as_reply_params(self, client):
    return ["{}={}".format(k, v) for k, v in self.support_params.items()] + \
        ["are supported by this server"]


def welcome(client, server):
  client.send_reply(Welcome())
  client.send_reply(YourHost())
  client.send_reply(Created())
  client.send_reply(MyInfo())
  client.send_reply(ISupport(server.isupport))

  client.on_message(client.hostmask, lusers.LUsers())
  client.on_message(client.hostmask, motd.Motd())

  if client.modes:
    flags, args = util.show_modes(client.modes)
    client.relay_to_self(mode.Mode(client.nickname, flags, *args))


@message.Command.register
class Nick(message.Command):
  NAME = "NICK"
  MIN_ARITY = 1

  def __init__(self, nickname, *args):
    self.nickname = nickname

  def as_params(self, client):
    return [self.nickname]

  def handle_for(self, client, prefix):
    old_hostmask = client.hostmask

    client.rename(self.nickname)

    if client.is_registered:
      client.relay_to_all(Nick(self.nickname), old_hostmask)
      client.relay_to_self(Nick(self.nickname), old_hostmask)
    else:
      if client.is_ready_for_registration:
        client.register()


@message.Command.register
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


@message.Command.register
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
