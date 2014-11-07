from merc import feature
from merc import message


class DebugFeature(feature.Feature):
  NAME = __name__


install = DebugFeature.install


@DebugFeature.register_user_command
class Debug(message.Command):
  NAME = "DEBUG"
  MIN_ARITY = 1

  def __init__(self, code, *args):
    self.code = code

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    user.check_is_irc_operator()
    try:
      result = repr(eval(self.code, {"app": app}))
    except BaseException as e:
      result = "{}: {}".format(e.__class__.__name__, e)

    for line in result.splitlines():
      app.run_hooks("server.notify", user, "DEBUG: {}".format(line))
