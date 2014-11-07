from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import mode


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
  def handle_for(self, app, user, prefix):
    for target_name in self.targets[:MAX_TARGETS]:
      if channel.Channel.is_channel_name(target_name):
        try:
          chan = app.channels.get(target_name)
        except errors.NoSuchNick:
          continue

        if DisallowingExternalMessages(chan).get():
          try:
            chan.check_has_user(user)
          except errors.NoSuchNick:
            raise errors.CannotSendToChan(chan.name)

          app.run_hooks("channel.message.check", user, chan)

        if Moderated(chan).get():
          chan.check_is_voiced(user)

        app.run_hooks("channel.message", user, chan, self.text)
        chan.broadcast(user, user.prefix, self.__class__(chan.name, self.text))
      else:
        target = app.users.get(target_name)
        app.run_hooks("user.message", user, target, self.text)
        target.send(user.prefix, self.__class__(target.nickname, self.text))


@PrivmsgFeature.register_user_command
class Privmsg(_Privmsg):
  NAME = "PRIVMSG"


@PrivmsgFeature.register_user_command
class Notice(_Privmsg):
  NAME = "NOTICE"


@PrivmsgFeature.register_channel_mode
class DisallowingExternalMessages(mode.FlagMode, mode.ChanModeMixin):
  CHAR = "n"
  DEFAULT = True


@PrivmsgFeature.register_channel_mode
class Moderated(mode.FlagMode, mode.ChanModeMixin):
  CHAR = "m"


@PrivmsgFeature.hook("server.notify")
def send_server_notice(app, user, text):
  user.send_reply(Notice("*", text))


@PrivmsgFeature.hook("server.targmax.modify")
def modify_targmax(app, targmax):
  targmax["PRIVMSG"] = MAX_TARGETS
  targmax["NOTICE"] = MAX_TARGETS
