from merc import feature
from merc import message


class PingFeature(feature.Feature):
  NAME = __name__


install = PingFeature.install


@PingFeature.register_server_command
class Ping(message.Command):
  NAME = "PING"
  MIN_ARITY = 2

  def __init__(self, source, destination, *args):
    self.source = source
    self.destination = destination

  def as_command_params(self):
    return [self.source, self.destination]

  def handle_for(self, app, server, prefix):
    target = app.users.get_by_uid(prefix)

    if self.destination == app.network.local.sid:
      app.network.get(target.server_name).send(
          app.network.local.sid,
          Pong(prefix, self.source))
    else:
      app.network.get_by_sid(self.destination).send(prefix, self)


@PingFeature.register_server_command
class Pong(message.Command):
  NAME = "PONG"
  MIN_ARITY = 2

  @property
  def sid(self):
    return self.source[:3]

  def __init__(self, source, destination, *args):
    self.source = source
    self.destination = destination

  def as_command_params(self):
    return [self.source, self.destination]

  def handle_for(self, app, server, prefix):
    if self.sid == app.network.local.sid:
      app.run_hooks("user.pong", app.users.get_by_uid(self.source),
                    app.network.get_by_sid(prefix).name,
                    self.destination)
    else:
      app.network.get_by_sid(self.sid).send(prefix, self)


@PingFeature.hook("server.ping")
def send_ping(app, user, source, destination):
  destination.send(user.link_prefix, Ping(source, destination.sid))
