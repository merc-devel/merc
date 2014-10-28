import merc

from merc import errors
from merc import feature
from merc import message
from merc import util


class UserFeature(feature.Feature):
  NAME = __name__


install = UserFeature.install


class Welcome(message.Reply):
  NAME = "001"
  FORCE_TRAILING = True

  def __init__(self, network_name, nickname):
    self.network_name = network_name
    self.nickname = nickname

  def as_reply_params(self):
    return ["Welcome to the {} Internet Relay Chat Network, {}".format(
        self.network_name,
        self.nickname)]


class YourHost(message.Reply):
  NAME = "002"
  FORCE_TRAILING = True

  def __init__(self, server_name, server_version):
    self.server_name = server_name
    self.server_version = server_version

  def as_reply_params(self):
    return ["Your host is {}, running {}-{}".format(
        self.server_name, merc.__name__, self.server_version)]


class Created(message.Reply):
  NAME = "003"
  FORCE_TRAILING = True

  def __init__(self, creation_time):
    self.creation_time = creation_time

  def as_reply_params(self):
    return ["This app was created {}".format(
        self.creation_time.isoformat())]


class MyInfo(message.Reply):
  NAME = "004"

  def __init__(self, server_name, umodes, chanmodes):
    self.server_name = server_name
    self.umodes = umodes
    self.chanmodes = chanmodes

  def as_reply_params(self):
    return [self.server_name,
            "{}-{}".format(merc.__name__, merc.__version__),
            "".join(sorted(mode.CHAR for mode in self.umodes.values())),
            "".join(sorted(mode.CHAR for mode in self.chanmodes.values()
                                     if not mode.TAKES_PARAM)),
            "".join(sorted(mode.CHAR for mode in self.chanmodes.values()
                                     if mode.TAKES_PARAM))]


@UserFeature.register_user_command
class User(message.Command):
  NAME = "USER"
  MIN_ARITY = 4

  def __init__(self, username, hostname, servername, realname, *args):
    self.username = username
    self.hostname = hostname
    self.servername = servername
    self.realname = realname

  def as_command_params(self):
    return [self.username, self.hostname, self.servername, self.realname]

  def handle_for(self, app, user, prefix):
    if user.is_registered:
      raise errors.AlreadyRegistered

    user.username = self.username
    user.realname = self.realname

    if user.is_ready_for_registration:
      user.register(app)


@UserFeature.hook("user.register")
def welcome_on_register(app, user):
  user.send_reply(Welcome(app.network_name, user.nickname))
  user.send_reply(YourHost(app.server_name, app.version))
  user.send_reply(Created(app.creation_time))
  user.send_reply(MyInfo(app.server_name, app.users.modes,
                         app.channels.modes))
  app.run_hooks("server.isupport.send", user)
  app.run_hooks("user.welcome", user)
