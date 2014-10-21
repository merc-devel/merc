import datetime
from merc import message


@message.Command.register
class Ping(message.Command):
  NAME = "PING"
  MIN_ARITY = 1

  def __init__(self, value, server_name=None, *args):
    self.value = value
    self.server_name = server_name

  @property
  def FORCE_TRAILING(self):
    return self.server_name is not None

  def as_params(self, client):
    params = [self.value]
    if self.server_name is not None:
      params.append(self.server_name)
    return params

  def handle_for(self, client, prefix):
    client.send_reply(Pong(
        self.server_name if self.server_name is not None
                         else client.server.name,
        self.value))


@message.Command.register
class Pong(message.Command):
  NAME = "PONG"
  MIN_ARITY = 1

  def __init__(self, server_name, value=None, *args):
    self.server_name = server_name
    self.value = value

  def as_params(self, client):
    return [self.server_name, self.value]

  def handle_for(self, client, prefix):
    pass
