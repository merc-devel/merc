from merc import channel
from merc import errors
from merc import message
from merc import util
from .away import IsAway


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
      if channel.Channel.is_valid_name(target):
        try:
          chan = client.server.get_channel(target)
        except errors.NoSuchNick:
          continue

        if chan.is_disallowing_external_messages:
          try:
            chan.check_has_client(client)
          except errors.NoSuchNick:
            raise errors.CannotSendToChan(chan.name)

        if chan.is_moderated:
          chan.check_is_voiced(client)

        client.relay_to_channel(chan,
                                self.__class__(chan.name, self.text))
      else:
        user = client.server.get_client(target)
        if user.is_away:
          client.send_reply(IsAway(user.nickname, user.away_message))
        client.relay_to_client(user,
                               self.__class__(user.nickname, self.text))


@message.Command.register
class Privmsg(_Privmsg):
  NAME = "PRIVMSG"


@message.Command.register
class Notice(_Privmsg):
  NAME = "NOTICE"
