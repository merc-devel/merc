import itertools

from merc import errors
from merc import message
from merc import util
from merc.features import mode
from merc.features import names
from merc.features import topic


class CreationTime(message.Reply):
  NAME = "329"

  def __init__(self, channel_name, time):
    self.channel_name = channel_name
    self.time = time

  def as_reply_params(self, client):
    return [self.channel_name, str(int(self.time.timestamp()))]


class _Join(message.Command):
  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    user = self.get_joining_client(client)

    for channel_name, key in itertools.zip_longest(self.channel_names,
                                                   self.keys,
                                                   fillvalue=None):
      is_new = False

      try:
        channel = client.server.get_channel(channel_name)
      except errors.NoSuchNick:
        channel = client.server.new_channel(channel_name)
        is_new = True

      if channel.has_client(user):
        continue

      self.check_can_join(user, channel, key)

      channel.join(user)
      channel.broadcast(None, user.hostmask, Join(channel.name))

      if channel.topic is not None:
        user.on_message(user.hostmask, topic.Topic(channel.name))

      user.on_message(user.hostmask, names.Names(channel.name))
      user.send_reply(CreationTime(channel.name, channel.creation_time))

      if is_new and channel.modes:
        flags, args = util.show_modes(channel.modes)
        user.send_reply(mode.Mode(channel.name, flags, *args))


@message.Command.register
class Join(_Join):
  NAME = "JOIN"
  MIN_ARITY = 1

  def __init__(self, channel_names, keys=None, *args):
    self.channel_names = channel_names.split(",")
    self.keys = keys.split(",") if keys is not None else []

  def check_can_join(self, client, channel, key):
    return

  def get_joining_client(self, client):
    return client

  def as_params(self, client):
    params = [",".join(self.channel_names)]
    if self.keys:
      params.append(",".join(self.keys))
    return params


@message.Command.register
class SaJoin(_Join):
  NAME = "SAJOIN"
  MIN_ARITY = 2

  def __init__(self, nickname, channel_names, *args):
    self.nickname = nickname
    self.channel_names = channel_names.split(",")
    self.keys = []

  def check_can_join(self, client, channel, key):
    return

  def get_joining_client(self, client):
    return client.server.get_client(self.nickname)

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    client.check_is_irc_operator()
    super().handle_for(client, prefix)


class _Part(message.Command):
  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    user = self.get_parting_client(client)

    for channel_name in self.channel_names:
      try:
        channel = client.server.get_channel(channel_name)
      except errors.NoSuchNick:
        raise errors.NoSuchChannel(channel_name)
      else:
        channel.broadcast(None, user.hostmask,
                          Part(channel.name, self.reason))
        client.server.part_channel(user, channel_name)


@message.Command.register
class Part(_Part):
  NAME = "PART"
  MIN_ARITY = 1

  def __init__(self, channel_names, reason=None, *args):
    self.channel_names = channel_names.split(",")
    self.reason = reason

  @property
  def FORCE_TRAILING(self):
    return self.reason is not None

  def get_parting_client(self, client):
    return client

  def as_params(self, client):
    params = [",".join(self.channel_names)]
    if self.reason is not None:
      params.append(self.reason)
    return params


@message.Command.register
class SaPart(_Part):
  NAME = "SAPART"
  MIN_ARITY = 2

  def __init__(self, nickname, channel_names, reason=None, *args):
    self.nickname = nickname
    self.channel_names = channel_names.split(",")
    self.reason = reason

  def get_parting_client(self, client):
    return client.server.get_client(self.nickname)

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    client.check_is_irc_operator()
    super().handle_for(client, prefix)
