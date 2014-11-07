from merc import errors
from merc import feature
from merc import message


class ConnectFeature(feature.Feature):
  NAME = __name__


install = ConnectFeature.install


@ConnectFeature.register_user_command
class Connect(message.Command):
  NAME = "CONNECT"
  MIN_ARITY = 1

  def __init__(self, server_name, *args):
    self.server_name = server_name

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    user.check_is_irc_operator()
    try:
      app.network.connect(self.server_name)
    except KeyError:
      raise errors.NoSuchServer(self.server_name)
