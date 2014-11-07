import datetime

from merc import errors
from merc import feature
from merc import message


PING_TIMEOUT = datetime.timedelta(seconds=240)
PONG_TIMEOUT = datetime.timedelta(seconds=20)


class PingFeature(feature.Feature):
  NAME = __name__


install = PingFeature.install


@PingFeature.register_user_command
class Ping(message.Command):
  NAME = "PING"
  MIN_ARITY = 1

  def __init__(self, origin, destination=None, *args):
    self.origin = origin
    self.destination = destination

  def as_command_params(self):
    params = [self.origin]
    if self.destination is not None:
      params.append(self.destination)
    return params

  def handle_for(self, app, user, prefix):
    if self.destination == app.server.name or self.destination is None:
      send_pong(app,
                user, self.destination if self.destination is not None
                                       else app.server.name,
                self.origin)
      return

    if not app.network.has(self.destination):
      raise errors.NoSuchServer(self.destination)

    app.run_hooks("server.ping", user,
                  self.origin,
                  app.network.get(self.destination))


@PingFeature.register_user_command
class Pong(message.Command):
  NAME = "PONG"
  MIN_ARITY = 1

  def __init__(self, origin, destination=None, *args):
    self.origin = origin
    self.destination = destination

  def as_command_params(self):
    params = [self.origin]

    if self.destination is not None:
      params.append(self.destination)

    return params

  def handle_for(self, app, user, prefix):
    pass


class PongReply(message.Reply):
  NAME = "PONG"
  MIN_ARITY = 1

  def __init__(self, destination, origin=None, *args):
    self.destination = destination
    self.origin = origin

  def as_reply_params(self):
    params = [self.origin]

    if self.destination is not None:
      params.append(self.destination)

    return params


@PingFeature.hook("user.register")
def reschedule_ping_check(app, user):
  if not user.is_registered:
    return

  if user.ping_check_handle is not None:
    user.ping_check_handle.cancel()

  if user.pong_check_handle is not None:
    user.pong_check_handle.cancel()

  def ping_check():
    user.send(None, Ping(app.server.name))
    user.pong_check_handle = app.loop.call_later(
        PONG_TIMEOUT.total_seconds(), pong_check)

  def pong_check():
    user.close("Ping timeout: {} seconds".format(
        int(PING_TIMEOUT.total_seconds())))

  user.ping_check_handle = app.loop.call_later(
      PING_TIMEOUT.total_seconds(), ping_check)


@PingFeature.hook("user.command")
def reschedule_ping_check_after_message(app, user, message, prefix):
  reschedule_ping_check(app, user)


@PingFeature.hook("user.pong")
def send_pong(app, user, origin, destination):
  user.send_reply(PongReply(origin, destination))
