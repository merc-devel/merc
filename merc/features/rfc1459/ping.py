import datetime

from merc import feature
from merc import message


PING_TIMEOUT = datetime.timedelta(seconds=240)
PONG_TIMEOUT = datetime.timedelta(seconds=20)


class PingFeature(feature.Feature):
  NAME = __name__


install = PingFeature.install


@PingFeature.register_command
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

  def handle_for(self, app, user, prefix):
    user.send_reply(Pong(
        self.server_name if self.server_name is not None else app.name,
        self.value))


@PingFeature.register_command
class Pong(message.Command):
  NAME = "PONG"
  MIN_ARITY = 1

  @property
  def FORCE_TRAILING(self):
    return self.value is not None

  def __init__(self, server_name, value=None, *args):
    self.server_name = server_name
    self.value = value

  def as_command_params(self):
    params = [self.server_name]

    if self.value is not None:
      params.append(self.value)

    return params

  def handle_for(self, app, user, prefix):
    pass


@PingFeature.hook("after_register")
def reschedule_ping_check(app, user):
  if not user.is_registered:
    return

  if user.ping_check_handle is not None:
    user.ping_check_handle.cancel()

  if user.pong_check_handle is not None:
    user.pong_check_handle.cancel()

  def ping_check():
    user.send(None, Ping(app.name))
    user.pong_check_handle = app.loop.call_later(
        PONG_TIMEOUT.total_seconds(), pong_check)

  def pong_check():
    user.close("Ping timeout: {} seconds".format(
        int(PING_TIMEOUT.total_seconds())))

  user.ping_check_handle = app.loop.call_later(
      PING_TIMEOUT.total_seconds(), ping_check)


@PingFeature.hook("after_message")
def reschedule_ping_check_after_message(app, user, message, prefix):
  reschedule_ping_check(app, user)
