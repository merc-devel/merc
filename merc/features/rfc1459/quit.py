from merc import feature
from merc import message


class QuitFeature(feature.Feature):
  NAME = __name__


install = QuitFeature.install


@QuitFeature.register_user_command
class Quit(message.Command):
  NAME = "QUIT"
  MIN_ARITY = 0

  def __init__(self, reason=None, *args):
    self.reason = reason

  @property
  def FORCE_TRAILING(self):
    return self.reason is not None

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    user.close("Quit: " + self.reason if self.reason is not None else None)

  def as_command_params(self):
    params = []
    if self.reason is not None:
      params.append(self.reason)
    return params


@QuitFeature.hook("before_remove_user")
def broadcast_quit_on_quit(app, user):
  user.relay_to_all(Quit(user.protocol.disconnect_reason))
