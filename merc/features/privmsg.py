from merc import errors
from merc import message
from merc import util


class _Privmsg(message.Command):
  MIN_ARITY = 2
  FORCE_TRAILING = True

  def __init__(self, targets, text, *args):
    self.targets = targets.split(",")
    self.text = text

  def as_params(self, client):
    return [",".join(self.targets), self.text]

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    for target in self.targets:
      if util.is_channel_name(target):
        try:
          channel = client.server.get_channel(target)
        except errors.NoSuchNick:
          continue

        if channel.is_disallowing_external_messages:
          channel.check_has_client(client)

        if channel.is_moderated:
          channel.check_is_voiced(client)

        client.relay_to_channel(channel,
                                self.__class__(channel.name, self.text))
      else:
        user = client.server.get_client(target)
        client.relay_to_client(user,
                               self.__class__(user.nickname, self.text))


@message.Command.register
class Privmsg(_Privmsg):
  NAME = "PRIVMSG"


@message.Command.register
class Notice(_Privmsg):
  NAME = "NOTICE"
