from merc import channel
from merc import errors
from merc import message
from merc import util


class EndOfWho(message.Reply):
  NAME = "315"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["End of /WHO command"]


class WhoReply(message.Reply):
  NAME = "352"
  FORCE_TRAILING = True

  def __init__(self, channel_name, user, hopcount, server):
    self.channel_name = channel_name
    self.user = user
    self.hopcount = hopcount
    self.server = server

  def as_reply_params(self, client):
    return [self.channel_name if self.channel_name is not None else "*",
            self.user.username, self.user.host, self.server, self.user.nickname,
            "H" if not self.user.is_away else "G",
            str(self.hopcount) + " " + self.user.realname]


@message.Command.register
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
                 if self.client_matches_query_type(user)]
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
        client.send_reply(WhoReply(channel_name, user, 0,
                                           client.server.name))

    client.send_reply(EndOfWho())

