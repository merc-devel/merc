from merc import feature
from merc import message


class MotdFeature(feature.Feature):
  NAME = __name__


install = MotdFeature.install


class MotdReply(message.Reply):
  NAME = "372"
  FORCE_TRAILING = True

  def __init__(self, line):
    self.line = line

  def as_reply_params(self, user):
    return ["- {}".format(self.line)]


class MotdStart(message.Reply):
  NAME = "375"
  FORCE_TRAILING = True

  def as_reply_params(self, user):
    return ["- {} Message of the Day".format(user.server.name)]


class EndOfMotd(message.Reply):
  NAME = "376"
  FORCE_TRAILING = True

  def as_reply_params(self, user):
    return ["End of /MOTD command"]


@MotdFeature.register_command
class Motd(message.Command):
  NAME = "MOTD"
  MIN_ARITY = 0

  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    user.send_reply(MotdStart())

    for line in user.server.motd.split("\n"):
      user.send_reply(MotdReply(line))

    user.send_reply(EndOfMotd())


@MotdFeature.hook("after_welcome")
def send_motd_on_welcome(user):
  user.on_message(user.hostmask, Motd())
