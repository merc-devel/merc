from merc import errors
from merc import message


class NameReply(message.Reply):
  NAME = "353"
  FORCE_TRAILING = True

  def __init__(self, type, channel_name, users):
    self.type = type
    self.channel_name = channel_name
    self.users = users

  def as_reply_params(self, client):
    return [self.type,
            self.channel_name if self.channel_name is not None else "*",
            " ".join(user.sigil + user.client.nickname for user in self.users)]


class EndOfNames(message.Reply):
  NAME = "366"
  FORCE_TRAILING = True

  def __init__(self, channel_name=None):
    self.channel_name = channel_name

  def as_reply_params(self, client):
    return [self.channel_name if self.channel_name is not None else "*",
            "End of /NAMES list"]


@message.Command.register
class Names(message.Command):
  NAME = "NAMES"
  MIN_ARITY = 0

  def __init__(self, channel_names=None, *args):
    self.channel_names = channel_names.split(",") if channel_names is not None \
                                                  else None

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    if self.channel_names is None:
      seen_nicknames = set()

      for chan in client.server.channels.values():
        if client.is_in_channel(chan):
          client.send_reply(NameReply(
                "@", chan.name, chan.get_visible_users_for(client)))
          continue

        if chan.is_secret:
          continue

        channel_users = []

        for user in chan.users.values():
          if not user.client.is_invisible:
            seen_nicknames.add(user.client.normalized_nickname)
            channel_users.append(user)

        if channel_users:
          client.send_reply(NameReply("=", chan.name, channel_users))

      visible_users = []

      for user in client.server.clients.values():
        if user.is_invisible and user is not client:
          continue

        if user.normalized_nickname not in seen_nicknames:
          visible_users.append(channel.ChannelUser(None, user))

      if visible_users:
        client.send_reply(NameReply("*", None, visible_users))

      client.send_reply(EndOfNames(None))
    else:
      for channel_name in self.channel_names:
        try:
          chan = client.server.get_channel(channel_name)
        except errors.NoSuchNick:
          pass
        else:
          if not client.can_see_channel(chan):
            continue

          channel_name = chan.name

          client.send_reply(NameReply(
              "@" if client.is_in_channel(chan) else "*",
              chan.name,
              chan.get_visible_users_for(client)))
        client.send_reply(EndOfNames(channel_name))

  def as_params(self, client):
    return [",".join(self.channel_names)]

