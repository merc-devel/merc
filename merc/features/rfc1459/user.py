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
  MIN_ARITY = 1

  def __init__(self, reason, *args):
    self.reason = reason

  def as_reply_params(self):
    return [self.reason]


class YourHost(message.Reply):
  NAME = "002"
  FORCE_TRAILING = True
  MIN_ARITY = 1

  def __init__(self, reason, *args):
    self.reason = reason

  def as_reply_params(self):
    return [self.reason]


class Created(message.Reply):
  NAME = "003"
  FORCE_TRAILING = True
  MIN_ARITY = 1

  def __init__(self, reason, *args):
    self.reason = reason

  def as_reply_params(self):
    return [self.reason]


class MyInfo(message.Reply):
  NAME = "004"
  MIN_ARITY = 5

  def __init__(self, server_name, version, umodes, chanmodes_no_param,
               chanmodes_param, *args):
    self.server_name = server_name
    self.version = version
    self.umodes = umodes
    self.chanmodes_no_param = chanmodes_no_param
    self.chanmodes_param = chanmodes_param

  def as_reply_params(self):
    return [self.server_name, self.version, self.umodes,
            self.chanmodes_no_param, self.chanmodes_param]


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
  user.send_reply(Welcome(
      "Welcome to the {} Internet Relay Chat Network, {}".format(
          app.network_name, user.nickname)))
  user.send_reply(YourHost("Your host is {}, running {}-{}".format(
      app.server.name, merc.__name__, app.version)))
  user.send_reply(Created("This server was created {}".format(
      app.creation_time.isoformat())))
  user.send_reply(MyInfo(
      app.server.name, "{}-{}".format(merc.__name__, merc.__version__),
      "".join(sorted(mode.CHAR for mode in app.users.modes.values())),
      "".join(sorted(mode.CHAR for mode in app.channels.modes.values()
                               if not mode.TAKES_PARAM)),
      "".join(sorted(mode.CHAR for mode in app.channels.modes.values()
                               if mode.TAKES_PARAM))))
  app.run_hooks("server.isupport.send", user)
  app.run_hooks("user.welcome", user)
