from merc import errors
from merc import feature
from merc import message


class NumericFeature(feature.Feature):
  NAME = __name__


install = NumericFeature.install


@NumericFeature.register_server_numeric_command
class Numeric(message.Command):
  MIN_ARITY = 2

  def __init__(self, name, target, *args):
    self.name = name
    self.target = target
    self.args = list(args)

  def as_command_params(self):
    return [self.name, self.target] + self.args

  @message.Command.requires_registration
  def handle_for(self, app, server, prefix):
    target = app.users.get_by_uid(self.target)
    target.send_reply(self, app.network.get_by_sid(prefix).name)
