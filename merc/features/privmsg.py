from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import mode
from merc import util


class PrivmsgFeature(feature.Feature):
  pass


install = PrivmsgFeature


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

        client.relay_to_channel(chan, self.__class__(chan.name, self.text))
      else:
        user = client.server.get_client(target)
        client.server.run_hooks("after_user_privmsg", client, user)
        client.relay_to_client(user, self.__class__(user.nickname, self.text))


@PrivmsgFeature.register_command
class Privmsg(_Privmsg):
  NAME = "PRIVMSG"


@PrivmsgFeature.register_command
class Notice(_Privmsg):
  NAME = "NOTICE"


@PrivmsgFeature.register_channel_mode
class DisallowingExternalMessages(mode.FlagMode):
  CHAR = "n"
  DEFAULT = True


@PrivmsgFeature.register_channel_mode
class Moderated(mode.FlagMode):
  CHAR = "m"
