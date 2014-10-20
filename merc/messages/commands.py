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
