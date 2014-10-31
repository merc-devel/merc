import datetime

from merc import errors
from merc import feature
from merc import message


class WhoIsFeature(feature.Feature):
  NAME = __name__


install = WhoIsFeature.install


MAX_TARGETS = 1


@WhoIsFeature.register_server_command
class WhoIsUser(message.Reply):
  NAME = "311"
  FORCE_TRAILING = True
  MIN_ARITY = 5

  def __init__(self, nick, user, host, star, realname, *args):
    self.nick = nick
    self.user = user
    self.host = host
    self.star = star
    self.realname = realname

  def as_reply_params(self):
    return [self.nick, self.user, self.host, self.star, self.realname]


@WhoIsFeature.register_server_command
class WhoIsServer(message.Reply):
  NAME = "312"
  FORCE_TRAILING = True
  MIN_ARITY = 3

  def __init__(self, nick, server_name, server_info="", *args):
    self.nick = nick
    self.server_name = server_name
    self.server_info = server_info

  def as_reply_params(self):
    return [self.nick, self.server_name, self.server_info]


@WhoIsFeature.register_server_command
class WhoIsOperator(message.Reply):
  NAME = "313"
  FORCE_TRAILING = True
  MIN_ARITY = 2

  def __init__(self, nick, reason="is an IRC operator", *args):
    self.nick = nick
    self.reason = reason

  def as_reply_params(self):
    return [self.nick, self.reason]


@WhoIsFeature.register_server_command
class WhoIsIdle(message.Reply):
  NAME = "317"
  FORCE_TRAILING = True
  MIN_ARITY = 4

  def __init__(self, nick, idle_time, signon_time,
               reason="seconds idle, signon time", *args):
    self.nick = nick
    self.idle_time = idle_time
    self.signon_time = signon_time
    self.reason = reason

  def as_reply_params(self):
    return [self.nick, self.idle_time, self.signon_time, self.reason]


@WhoIsFeature.register_server_command
class WhoIsEnd(message.Reply):
  NAME = "318"
  FORCE_TRAILING = True
  MIN_ARITY = 2

  def __init__(self, nick, reason="End of /WHOIS list", *args):
    self.nick = nick
    self.reason = reason

  def as_reply_params(self):
    return [self.nick, self.reason]


@WhoIsFeature.register_server_command
class WhoIsChannels(message.Reply):
  NAME = "319"
  FORCE_TRAILING = True
  MIN_ARITY = 2

  def __init__(self, nick, channels, *args):
    self.nick = nick
    self.channels = channels

  def as_reply_params(self):
    return [self.nick, self.channels]


@WhoIsFeature.register_user_command
class WhoIs(message.Command):
  NAME = "WHOIS"
  MIN_ARITY = 1

  def __init__(self, nicknames, *args):
    self.nicknames = nicknames.split(",")

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    for nickname in self.nicknames[:MAX_TARGETS]:
      try:
        target = app.users.get(nickname)
      except errors.NoSuchNick as e:
        user.send_reply(e)
      else:
        channels = []
        for channel in user.get_channels_visible_for(target):
          sigil = channel.get_channel_user_for(target).sigil
          name = sigil + channel.name
          channels.append(name)

        user.send_reply(WhoIsUser(target.nickname, target.username,
                                  target.host, "*", target.realname))
        user.send_reply(WhoIsServer(target.nickname, target.server_name,
                                      app.network_name))
        if target.is_irc_operator:
          user.send_reply(WhoIsOperator(target.nickname))
        user.send_reply(WhoIsIdle(
            target.nickname,
            str(int((datetime.datetime.now() -
                target.last_activity_time).timestamp())),
            str(int(target.creation_time.timestamp()))))
        if channels:
          user.send_reply(WhoIsChannels(target.nickname, " ".join(channels)))
        app.run_hooks("user.whois", user, target)
        user.send_reply(WhoIsEnd(target.nickname))


@WhoIsFeature.hook("server.targmax.modify")
def modify_targmax(app, targmax):
  targmax["WHOIS"] = MAX_TARGETS
