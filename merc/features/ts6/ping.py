from merc import errors
from merc import feature
from merc import message


class PingFeature(feature.Feature):
  NAME = __name__


install = PingFeature.install


@PingFeature.register_server_command
class Ping(message.Command):
  NAME = "PING"
  MIN_ARITY = 1
  FORCE_TRAILING = True

  def __init__(self, value, server_name=None, *args):
    self.value = value
    self.server_name = server_name

  def as_command_params(self):
    params = [self.value]
    if self.server_name is not None:
      params.append(self.server_name)
    return params

  def handle_for(self, app, server, prefix):
    target = app.users.get_by_uid(prefix)

    if self.server_name == app.server.name:
      app.network.get(target.server_name).send(
          app.network.local.sid,
          Pong(prefix, app.server.name, self.value))
    else:
      send_ping(app, target, self.value, self.server_name)


@PingFeature.register_server_command
class Pong(message.Command):
  NAME = "PONG"
  MIN_ARITY = 2

  @property
  def FORCE_TRAILING(self):
    return self.value is not None

  @property
  def sid(self):
    return self.target[:3]

  def __init__(self, target, server_name, value=None, *args):
    self.target = target
    self.server_name = server_name
    self.value = value

  def as_command_params(self):
    params = [self.target, self.server_name]

    if self.value is not None:
      params.append(self.value)

    return params

  def handle_for(self, app, server, prefix):
    if self.sid == app.network.local.sid:
      app.run_hooks("user.pong", app.users.get_by_uid(self.target),
                    app.network.get_by_sid(prefix).name,
                    self.value)
    else:
      app.network.get_by_sid(self.sid).send(prefix, self)


@PingFeature.hook("server.ping")
def send_ping(app, user, value, server_name):
  app.network.get(server_name).send(user.link_prefix, Ping(value, server_name))
