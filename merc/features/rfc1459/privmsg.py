from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import mode
from merc import util


class PrivmsgFeature(feature.Feature):
  NAME = __name__


install = PrivmsgFeature.install


MAX_TARGETS = 4


class _Privmsg(message.Command):
  MIN_ARITY = 2
  FORCE_TRAILING = True

  def __init__(self, targets, text, *args):
    self.targets = targets.split(",")
    self.text = text

  def as_command_params(self):
    return [",".join(self.targets), self.text]

  @message.Command.requires_registration
  def handle_for(self, server, user, prefix):
    for target_name in self.targets[:MAX_TARGETS]:
      if channel.Channel.is_channel_name(target_name):
        try:
          chan = server.channels.get(target_name)
        except errors.NoSuchNick:
          continue

        if DisallowingExternalMessages(chan).get():
          try:
            chan.check_has_user(user)
          except errors.NoSuchNick:
            raise errors.CannotSendToChan(chan.name)

          server.run_hooks("check_can_message_channel", user, chan)

        if Moderated(chan).get():
          chan.check_is_voiced(user)

        server.run_hooks("after_channel_privmsg", user, chan)
        user.relay_to_channel(chan, self.__class__(chan.name, self.text))
      else:
        target = server.users.get(target_name)
        server.run_hooks("after_user_privmsg", user, target)
        user.relay_to_user(target, self.__class__(target.nickname, self.text))


@PrivmsgFeature.register_command
class Privmsg(_Privmsg):
  NAME = "PRIVMSG"


@PrivmsgFeature.register_command
class Notice(_Privmsg):
  NAME = "NOTICE"


@PrivmsgFeature.register_channel_mode
class DisallowingExternalMessages(mode.FlagMode, mode.ChanModeMixin):
  CHAR = "n"
  DEFAULT = True


@PrivmsgFeature.register_channel_mode
class Moderated(mode.FlagMode, mode.ChanModeMixin):
  CHAR = "m"


@PrivmsgFeature.hook("send_server_notice")
def send_server_notice(server, user, text):
  user.send_reply(Notice("*", text))


@PrivmsgFeature.hook("modify_targmax")
def modify_targmax(server, targmax):
  targmax["PRIVMSG"] = MAX_TARGETS
  targmax["NOTICE"] = MAX_TARGETS
