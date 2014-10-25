from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import util


class WhoFeature(feature.Feature):
  NAME = __name__


install = WhoFeature


class EndOfWho(message.Reply):
  NAME = "315"
  FORCE_TRAILING = True

  def __init__(self, target):
    self.target = target

  def as_reply_params(self, client):
    return [self.target, "End of /WHO command"]


class WhoReply(message.Reply):
  NAME = "352"
  FORCE_TRAILING = True

  def __init__(self, channel_name, username, host, server, nickname, is_away,
               hopcount, realname):
    self.channel_name = channel_name
    self.username = username
    self.host = host
    self.server = server
    self.nickname = nickname
    self.is_away = is_away
    self.hopcount = hopcount
    self.realname = realname

  def as_reply_params(self, client):
    return [self.channel_name if self.channel_name is not None else "*",
            self.username, self.host, self.server, self.nickname,
            "H" if not self.is_away else "G",
            str(self.hopcount) + " " + self.realname]


@WhoFeature.register_command
class Who(message.Command):
  NAME = "WHO"
  MIN_ARITY = 1

  def __init__(self, target, query_type=None, *args):
    self.target = target
    self.query_type = query_type

  def client_matches_query_type(self, user):
    if self.query_type == "o":
      return user.is_irc_operator
    return True

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    who = []

    try:
      if channel.Channel.is_valid_name(self.target):
        chan = client.server.get_channel(self.target)

        if client.can_see_channel(chan):
          who = [(chan.name, user.client)
                 for user in chan.get_visible_users_for(client)
                 if self.client_matches_query_type(user.client)]
      else:
        for user in client.server.query_clients(self.target):
          if not self.client_matches_query_type(user):
            continue

          visible_it = iter(client.get_channels_visible_for(user))

          try:
            visible_channel = next(visible_it)
          except StopIteration:
            visible_channel_name = None
          else:
            visible_channel_name = visible_channel.name

          who.append((visible_channel_name, user))
    except errors.NoSuchNick:
      pass

    for channel_name, user in who:
      reply = WhoReply(channel_name, user.username, user.host, user.server.name,
                       user.nickname, False, 0, user.realname)
      client.server.run_hooks("mutate_who_reply", user, reply)
      client.send_reply(reply)

    client.send_reply(EndOfWho(self.target))
