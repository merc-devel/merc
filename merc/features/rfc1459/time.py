import datetime

from merc import errors
from merc import feature
from merc import message


class TimeFeature(feature.Feature):
  NAME = __name__


install = TimeFeature.install


class TimeReply(message.Reply):
  NAME = "391"
  FORCE_TRAILING = True

  def __init__(self, server_name, time):
    self.server_name = server_name
    self.time = time

  def as_reply_params(self):
    return [self.server_name, self.time.isoformat()]


@TimeFeature.register_user_command
class Time(message.Command):
  NAME = "TIME"
  MIN_ARITY = 0

  def __init__(self, server_name=None, *args):
    self.server_name = server_name

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    if self.server_name is not None and self.server_name != app.server_name:
      raise errors.NoSuchServer(self.server_name)

    user.send_reply(TimeReply(app.server_name, datetime.datetime.now()))
