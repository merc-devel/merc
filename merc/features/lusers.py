from merc import feature
from merc import message


class LUsersFeature(feature.Feature):
  NAME = __name__


install = LUsersFeature


class LUserUnknown(message.Reply):
  NAME = "253"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["0", "unknown connections"]


class LUserChannels(message.Reply):
  NAME = "254"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return [str(len(client.server.channels)), "channels formed"]


class LUserMe(message.Reply):
  NAME = "255"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["I have {} clients and {} servers".format(
        len(client.server.clients),
        1)]


@LUsersFeature.register_command
class LUsers(message.Command):
  NAME = "LUSERS"
  MIN_ARITY = 0

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    client.server.run_hooks("luser_client", client)
    client.server.run_hooks("luser_oper", client)
    client.send_reply(LUserUnknown())
    client.send_reply(LUserChannels())
    client.send_reply(LUserMe())


@LUsersFeature.hook("after_welcome")
def send_lusers_on_welcome(client):
  client.on_message(client.hostmask, LUsers())
