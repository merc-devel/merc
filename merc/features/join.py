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


@message.Command.register
class Join(message.Command):
  NAME = "JOIN"
  MIN_ARITY = 1

  def __init__(self, channel_names, keys=None, *args):
    self.channel_names = channel_names.split(",")
    self.keys = keys.split(",") if keys is not None else []

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    for channel_name, key in itertools.zip_longest(self.channel_names,
                                                   self.keys,
                                                   fillvalue=None):
      is_new = False

      try:
        channel = client.server.get_channel(channel_name)
      except errors.NoSuchNick:
        channel = client.server.new_channel(channel_name)
        is_new = True

      if channel.has_client(client):
        continue

      channel.join(client)
      channel.broadcast(None, client.hostmask, Join(channel.name))

      if channel.topic is not None:
        client.on_message(client.hostmask, topic.Topic(channel.name))

      client.on_message(client.hostmask, names.Names(channel.name))
      client.send_reply(CreationTime(channel.name, channel.creation_time))

      if is_new and channel.modes:
        flags, args = util.show_modes(channel.modes)
        client.send_reply(mode.Mode(channel.name, flags, *args))

  def as_params(self, client):
    params = [",".join(self.channel_names)]
    if self.keys:
      params.append(",".join(self.keys))
    return params


@message.Command.register
class Part(message.Command):
  NAME = "PART"
  MIN_ARITY = 1

  def __init__(self, channel_names, reason=None, *args):
    self.channel_names = channel_names.split(",")
    self.reason = reason

  @property
  def FORCE_TRAILING(self):
    return self.reason is not None

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    for channel_name in self.channel_names:
      try:
        channel = client.server.get_channel(channel_name)
      except errors.NoSuchNick:
        raise errors.NoSuchChannel(channel_name)
      else:
        client.server.part_channel(client, channel_name)
        channel.broadcast(None, client.hostmask,
                          Part(channel.name, self.reason))

  def as_params(self, client):
    params = [",".join(self.channel_names)]
    if self.reason is not None:
      params.append(self.reason)
    return params

