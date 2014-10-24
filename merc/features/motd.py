from merc import feature
from merc import message


class MotdFeature(feature.Feature):
  pass


install = MotdFeature


class MotdReply(message.Reply):
  NAME = "372"
  FORCE_TRAILING = True

  def __init__(self, line):
    self.line = line

  def as_reply_params(self, client):
    return ["- {}".format(self.line)]


class MotdStart(message.Reply):
  NAME = "375"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["- {} Message of the Day".format(client.server.name)]


class EndOfMotd(message.Reply):
  NAME = "376"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["End of /MOTD command"]


@MotdFeature.register_command
class Motd(message.Command):
  NAME = "MOTD"
  MIN_ARITY = 0

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    client.send_reply(MotdStart())

    for line in client.server.motd.split("\n"):
      client.send_reply(MotdReply(line))

    client.send_reply(EndOfMotd())


@MotdFeature.hook("after_welcome")
def send_motd_on_welcome(client):
  client.on_message(client.hostmask, Motd())
