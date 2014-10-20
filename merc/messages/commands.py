import datetime
import functools
import itertools

from merc import util
from merc.messages import errors
from merc.messages import replies
from merc.messages import message


def requires_registration(f):
  @functools.wraps(f)
  def _wrapper(self, client, prefix):
    if client.is_registered:
      f(self, client, prefix)
  return _wrapper


class Command(message.Message):
  def __init__(self, *args):
    pass

  @classmethod
  def with_params(cls, params):
    from merc.messages import errors

    if len(params) < cls.MIN_ARITY:
      raise errors.NeedMoreParams(cls.NAME)

    return cls(*params)

  def handle_for(self, client, prefix):
    pass


class Ping(Command):
  NAME = "PING"
  MIN_ARITY = 1

  def __init__(self, value, server_name=None, *args):
    self.value = value
    self.server_name = server_name

  def as_params(self, client):
    return [self.value, self.server_name]

  @requires_registration
  def handle_for(self, client, prefix):
    client.send_reply(Pong(
        self.server_name if self.server_name is not None
                         else client.server.name,
        self.value))

class Pong(Command):
  NAME = "PONG"
  MIN_ARITY = 2

  def __init__(self, server_name, value, *args):
    self.server_name = server_name
    self.value = value

  def as_params(self, client):
    return [self.server_name, self.value]


class Nick(Command):
  NAME = "NICK"
  MIN_ARITY = 1

  def __init__(self, nickname, *args):
    self.nickname = nickname

  def as_params(self, client):
    return [self.nickname]

  def handle_for(self, client, prefix):
    old_hostmask = client.hostmask

    client.rename(self.nickname)

    if client.is_registered:
      client.relay_to_all(Nick(self.nickname), old_hostmask)
      client.relay_to_self(Nick(self.nickname), old_hostmask)
    else:
      if client.is_ready_for_registration:
        client.register()


class User(Command):
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

    if client.is_ready_for_registration:
      client.register()


class LUsers(Command):
  NAME = "LUSERS"
  MIN_ARITY = 0

  @requires_registration
  def handle_for(self, client, prefix):
    client.send_reply(replies.LUserClient())
    client.send_reply(replies.LUserOp())
    client.send_reply(replies.LUserUnknown())
    client.send_reply(replies.LUserChannels())
    client.send_reply(replies.LUserMe())


class Motd(Command):
  NAME = "MOTD"
  MIN_ARITY = 0

  @requires_registration
  def handle_for(self, client, prefix):
    client.send_reply(replies.MotdStart())

    for line in client.server.motd.split("\n"):
      client.send_reply(replies.Motd(line))

    client.send_reply(replies.EndOfMotd())


class Privmsg(Command):
  NAME = "PRIVMSG"
  MIN_ARITY = 2

  def __init__(self, channel_names, text, *args):
    self.channel_names = channel_names.split(",")
    self.text = text

  def as_params(self, client):
    return [",".join(self.channel_names), self.text]

  @requires_registration
  def handle_for(self, client, prefix):
    for channel_name in self.channel_names:
      if channel_name[0] == "#":
        channel = client.server.get_channel(channel_name)
        client.relay_to_channel(channel, Privmsg(channel_name, self.text))
      else:
        user = client.server.clients[util.to_irc_lower(channel_name)]
        client.relay_to_client(user, Privmsg(channel_name, self.text))


class Notice(Command):
  NAME = "NOTICE"
  MIN_ARITY = 2

  def __init__(self, channel_names, text, *args):
    self.channel_names = channel_names.split(",")
    self.text = text

  def as_params(self, client):
    return [",".join(self.channel_names), self.text]

  @requires_registration
  def handle_for(self, client, prefix):
    for channel_name in self.channel_names:
      if channel_name[0] == "#":
        channel = client.server.get_channel(channel_name)
        client.relay_to_channel(channel, Notice(channel_name, self.text))
      else:
        user = client.server.clients[util.to_irc_lower(channel_name)]
        client.relay_to_client(user, Notice(channel_name, self.text))


class Join(Command):
  NAME = "JOIN"
  MIN_ARITY = 1

  def __init__(self, channel_names, keys=None, *args):
    self.channel_names = channel_names.split(",")
    self.keys = keys.split(",") if keys is not None else []

  @requires_registration
  def handle_for(self, client, prefix):
    for channel_name, key in itertools.zip_longest(self.channel_names,
                                                   self.keys,
                                                   fillvalue=None):
      channel = client.server.join_channel(client, channel_name, key)

      client.relay_to_channel(channel, Join(channel.name))
      client.relay_to_self(Join(channel.name))

      client.on_message(client.hostmask, Names(channel.name))

      if channel.topic is not None:
        client.on_message(client.hostmask, Topic(channel.name))

  def as_params(self, client):
    params = [",".join(self.channel_names)]
    if self.keys:
      params.append(",".join(self.keys))
    return params


class Part(Command):
  NAME = "PART"
  MIN_ARITY = 1

  def __init__(self, channel_names, reason=None, *args):
    self.channel_names = channel_names.split(",")
    self.reason = reason

  @requires_registration
  def handle_for(self, client, prefix):
    for channel_name in self.channel_names:
      try:
        channel = client.server.get_channel(channel_name)
      except KeyError:
        client.send_reply(errors.NoSuchChannel(channel_name))
      else:
        client.server.part_channel(client, channel_name)
        client.relay_to_channel(channel, Part(channel.name, self.reason))
        client.relay_to_self(Part(channel.name, self.reason))

  def as_params(self, client):
    params = [",".join(self.channel_names)]
    if self.reason is not None:
      params.append(self.reason)
    return params


class Names(Command):
  NAME = "NAMES"
  MIN_ARITY = 0

  def __init__(self, channel_names=None, *args):
    self.channel_names = channel_names.split(",") if channel_names is not None \
                                                  else None

  @requires_registration
  def handle_for(self, client, prefix):
    if self.channel_names is None:
      client.send_reply(replies.EndOfNames(None))
    else:
      for channel_name in self.channel_names:
        try:
          channel = client.server.get_channel(channel_name)
        except KeyError:
          client.send_reply(errors.NoSuchChannel(channel_name))
        else:
          client.send_reply(replies.NameReply("@", channel.name, channel.users))
          client.send_reply(replies.EndOfNames(channel.name))

  def as_params(self, client):
    return [",".join(self.channel_names)]


class Topic(Command):
  NAME = "TOPIC"
  MIN_ARITY = 1

  def __init__(self, channel_name, text=None):
    self.channel_name = channel_name
    self.text = text

  @requires_registration
  def handle_for(self, client, prefix):
    channel = client.server.get_channel(self.channel_name)

    if self.text is None:
      if channel.topic is not None:
        client.send_reply(replies.Topic(channel.name, channel.topic.text))
        client.send_reply(replies.TopicWhoTime(channel.name, channel.topic.who,
                                               channel.topic.time))
      else:
        client.send_reply(replies.NoTopic(channel.name))
    else:
      channel.set_topic(client, self.text)

      client.relay_to_channel(channel, Topic(channel.name, channel.topic.text))
      client.relay_to_self(Topic(channel.name, channel.topic.text))

  def as_params(self, client):
    params = [self.channel_name]
    if self.text is not None:
      params.append(self.text)
    return params


class Quit(Command):
  NAME = "QUIT"
  MIN_ARITY = 0

  def __init__(self, reason=None):
    self.reason = reason

  @requires_registration
  def handle_for(self, client, prefix):
    client.close("Quit: " + self.reason if self.reason is not None
                                        else "Remote host closed connection")

  def as_params(self, client):
    params = []
    if self.reason is not None:
      params.append(self.reason)
    return params
