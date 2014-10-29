from merc import errors
from merc import feature
from merc import message


class ServerFeature(feature.Feature):
  NAME = __name__


install = ServerFeature.install


@ServerFeature.register_server_command
class Server(message.Command):
  NAME = "SERVER"
  MIN_ARITY = 3

  def __init__(self, name, hopcount, description, *args):
    self.name = name
    self.hopcount = hopcount
    self.description = description

  def handle_for(self, app, server, prefix):
    if server.is_registered:
      return

    try:
      server.hopcount = int(self.hopcount)
    except ValueError:
      raise errors.LinkError("Hopcount is not valid")

    server.name = self.name
    server.description = self.description

    server.register(app)

  def as_command_params(self):
    return [self.name, self.hopcount, self.description]


@ServerFeature.hook("server.server")
def send_server(app, server, name, hopcount, description):
  server.send(None, Server(name, hopcount, description))
