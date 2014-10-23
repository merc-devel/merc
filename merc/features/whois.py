from merc import errors
from merc import message
from merc.features import away


class WhoIsUser(message.Reply):
  NAME = "311"
  FORCE_TRAILING = True

  def __init__(self, nick, user, host, realname):
    self.nick = nick
    self.user = user
    self.host = host
    self.realname = realname

  def as_reply_params(self, client):
    return [self.nick, self.user, self.host, "*", self.realname]

class WhoIsServer(message.Reply):
  NAME = "312"
  FORCE_TRAILING = True

  def __init__(self, nick, server, server_info=""):
    self.nick = nick
    self.server = server
    self.server_info = server_info

  def as_reply_params(self, client):
    return [self.nick, self.server, self.server_info]

class WhoIsOperator(message.Reply):
  NAME = "313"
  FORCE_TRAILING = True

  def __init__(self, nick):
    self.nick = nick

  def as_reply_params(self, client):
    return [self.nick, "is an IRC operator"]

class WhoIsSecure(message.Reply):
  NAME = "671"
  FORCE_TRAILING = True

  def __init__(self, nick, type):
    self.nick = nick
    self.type = type

  def as_reply_params(self, client):
    return [self.nick, self.type, "is using a secure connection"]

class WhoIsIdle(message.Reply):
  NAME = "317"
  FORCE_TRAILING = True

  def __init__(self, nick, idle_time):
    self.nick = nick
    self.idle_time = idle_time

  def as_reply_params(self, client):
    return [self.nick, str(round(self.idle_time)), "seconds idle"]

class WhoIsEnd(message.Reply):
  NAME = "318"
  FORCE_TRAILING = True

  def __init__(self, nick):
    self.nick = nick

  def as_reply_params(self, client):
    return [self.nick, "End of /WHOIS list"]

class WhoIsChannels(message.Reply):
  NAME = "319"
  FORCE_TRAILING = True

  def __init__(self, nick, channels):
    self.nick = nick
    self.channels = channels

  def as_reply_params(self, client):
    return [self.nick, " ".join(self.channels)]


@message.Command.register
class WhoIs(message.Command):
  NAME = "WHOIS"
  MIN_ARITY = 1

  def __init__(self, nicknames, *args):
    self.nicknames = nicknames.split(",")

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    for nickname in self.nicknames:
      try:
        target = client.server.get_client(nickname)
      except errors.NoSuchNick as e:
        client.send_reply(e)
      else:
        idle_time = (target.last_activity_time - target.creation_time).total_seconds()
        channels = []
        for channel in client.get_channels_visible_for(target):
          sigil = channel.get_channel_user_for(target).sigil
          name = sigil + channel.name
          channels.append(name)

        client.send_reply(WhoIsUser(target.nickname, target.username, target.host, target.realname))
        client.send_reply(WhoIsServer(target.nickname, target.server.name, target.server.network_name))
        if target.is_irc_operator:
          client.send_reply(WhoIsOperator(target.nickname))
        client.send_reply(WhoIsIdle(target.nickname, idle_time))
        if channels:
          client.send_reply(WhoIsChannels(target.nickname, channels))
        if target.is_securely_connected:
          client.send_reply(WhoIsSecure(target.nickname, "*"))
        if target.is_away:
          client.send_reply(away.IsAway(target.nickname, target.away_message))
        client.send_reply(WhoIsEnd(target.nickname))
