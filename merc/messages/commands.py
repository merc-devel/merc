import itertools

from merc.messages import errors
from merc.messages import message


class Nick(message.Message):
  NAME = "NICK"
  MIN_ARITY = 1

  def __init__(self, nickname, *args):
    self.nickname = nickname

  def as_params(self, client):
    return [self.nickname]

  def handle_for(self, client, prefix):
    client.rename(self.nickname)

    if client.username is not None:
      client.register()


class User(message.Message):
  NAME = "USER"
  MIN_ARITY = 4

  def __init__(self, user, hostname, servername, realname, *args):
    self.user = user
    self.hostname = hostname
    self.servername = servername
    self.realname = realname

  def as_params(self, client):
    return [self.user, self.hostname, self.servername, self.realname]

  def handle_for(self, client, prefix):
    client.username = self.user
    client.realname = self.realname

    if client.nickname is not None:
      client.register()


class Privmsg(message.Message):
  NAME = "PRIVMSG"
  MIN_ARITY = 2

  def __init__(self, channel, text, *args):
    self.channel = channel
    self.text = text

  def as_params(self, client):
    return [self.channel, self.text]


class Notice(message.Message):
  NAME = "NOTICE"
  MIN_ARITY = 2

  def __init__(self, channel, text, *args):
    self.channel = channel
    self.text = text

  def as_params(self, client):
    return [self.channel, self.text]


class Join(message.Message):
  NAME = "JOIN"
  MIN_ARITY = 1

  def __init__(self, channel_names, keys=None, *args):
    self.channel_names = channel_names.split(",")
    self.keys = keys.split(",") if keys is not None else []

  def handle_for(self, client, prefix):
    for channel_name, key in itertools.zip_longest(self.channel_names,
                                                   self.keys,
                                                   fillvalue=None):
      channel = client.server.join_channel(client, channel_name, key)
      client.relay_to_channel(channel, Join(channel.name))

  def as_params(self, client):
    params = [",".join(self.channel_names)]
    if self.keys:
      params.append(",".join(self.keys))
    return params
