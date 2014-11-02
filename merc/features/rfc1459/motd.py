from merc import config
from merc import feature
from merc import message


class MotdFeature(feature.Feature):
  NAME = __name__
  CONFIG_SECTION = 'motd'


install = MotdFeature.install


@MotdFeature.register_config_checker
def check_config(section):
  return config.validate(section, str)


@MotdFeature.register_server_command
class MotdReply(message.Reply):
  NAME = "372"
  FORCE_TRAILING = True
  MIN_ARITY = 1

  def __init__(self, line, *args):
    self.line = line

  def as_reply_params(self):
    return [self.line]


@MotdFeature.register_server_command
class MotdStart(message.Reply):
  NAME = "375"
  FORCE_TRAILING = True
  MIN_ARITY = 1

  def __init__(self, reason, *args):
    self.reason = reason

  def as_reply_params(self):
    return [self.reason]


@MotdFeature.register_server_command
class EndOfMotd(message.Reply):
  NAME = "376"
  FORCE_TRAILING = True
  MIN_ARITY = 1

  def __init__(self, reason="End of /MOTD command", *args):
    self.reason = reason

  def as_reply_params(self):
    return [self.reason]


@MotdFeature.register_user_command
class Motd(message.Command):
  NAME = "MOTD"
  MIN_ARITY = 0

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    motd = app.features.get_config_section(__name__)
    user.send_reply(MotdStart(
        "- {} Message of the Day".format(app.server.name)))

    for line in motd.splitlines():
      user.send_reply(MotdReply("- " + line))

    user.send_reply(EndOfMotd())


@MotdFeature.hook("user.welcome")
def send_motd_on_welcome(app, user):
  user.on_message(app, user.hostmask, Motd())
