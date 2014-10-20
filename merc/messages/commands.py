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
    if not client.is_registered:
      raise errors.NotRegistered

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
  FORCE_TRAILING = True

  def __init__(self, targets, text, *args):
    self.targets = targets.split(",")
    self.text = text

  def as_params(self, client):
    return [",".join(self.targets), self.text]

  @requires_registration
  def handle_for(self, client, prefix):
    for target in self.targets:
      if util.is_channel_name(target):
        channel = client.server.get_channel(target)
        client.relay_to_channel(channel, Privmsg(target, self.text))
      else:
        user = client.server.get_client(target)
        client.relay_to_client(user, Privmsg(target, self.text))


class Notice(Command):
  NAME = "NOTICE"
  MIN_ARITY = 2
  FORCE_TRAILING = True

  def __init__(self, targets, text, *args):
    self.targets = targets.split(",")
    self.text = text

  def as_params(self, client):
    return [",".join(self.targets), self.text]

  @requires_registration
  def handle_for(self, client, prefix):
    for target in self.targets:
      if util.is_channel_name(target):
        channel = client.server.get_channel(target)
        client.relay_to_channel(channel, Notice(target, self.text))
      else:
        user = client.server.get_client(target)
        client.relay_to_client(user, Notice(target, self.text))


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
      channel = client.server.get_or_new_channel(channel_name)

      if channel.has_client(client):
        continue

      channel.join(client)

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

  @property
  def FORCE_TRAILING(self):
    return self.reason is not None

  @requires_registration
  def handle_for(self, client, prefix):
    for channel_name in self.channel_names:
      try:
        channel = client.server.get_channel(channel_name)
      except errors.NoSuchNick:
        raise errors.NoSuchChannel(channel_name)
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
        except errors.NoSuchNick:
          pass
        else:
          channel_name = channel.name
          client.send_reply(replies.NameReply("@", channel.name, channel.users))
        client.send_reply(replies.EndOfNames(channel_name))

  def as_params(self, client):
    return [",".join(self.channel_names)]


class Topic(Command):
  NAME = "TOPIC"
  MIN_ARITY = 1

  def __init__(self, channel_name, text=None, *args):
    self.channel_name = channel_name
    self.text = text

  @property
  def FORCE_TRAILING(self):
    return self.text is not None

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

  def __init__(self, reason=None, *args):
    self.reason = reason

  @property
  def FORCE_TRAILING(self):
    return self.reason is not None

  @requires_registration
  def handle_for(self, client, prefix):
    client.close("Quit: " + self.reason if self.reason is not None
                                        else "Remote host closed connection")

  def as_params(self, client):
    params = []
    if self.reason is not None:
      params.append(self.reason)
    return params


class IsOn(Command):
  NAME = "ISON"
  MIN_ARITY = 0

  def __init__(self, *nicknames):
    self.nicknames = nicknames

  @requires_registration
  def handle_for(self, client, prefix):
    is_on = []
    for nickname in self.nicknames:
      try:
        client.server.get_client(nickname)
      except errors.NoSuchNick:
        pass
      else:
        is_on.append(nickname)

    client.send_reply(replies.IsOn(is_on))


class UserHost(Command):
  NAME = "USERHOST"
  MIN_ARITY = 1

  def __init__(self, *nicknames):
    self.nicknames = nicknames[:5]

  @requires_registration
  def handle_for(self, client, prefix):
    user_hosts = []
    for nickname in self.nicknames:
      try:
        user = client.server.get_client(nickname)
      except errors.NoSuchNick:
        pass
      else:
        user_hosts.append("{}={}{}".format(
            nickname, "-" if user.is_away else "+", user.hostmask))

    client.send_reply(replies.UserHost(user_hosts))


class Mode(Command):
  NAME = "MODE"
  MIN_ARITY = 1

  def __init__(self, target, flags=None, *args):
    self.target = target
    self.flags = flags
    self.args = args

  def as_params(self, client):
    return [self.target, self.flags] + list(self.args)

  @requires_registration
  def handle_for(self, client, prefix):
    if self.flags is None:
      if util.is_channel_name(self.target):
        try:
          channel = client.server.get_channel(self.target)
        except errors.NoSuchNick:
          raise errors.NoSuchChannel(self.target)

        client.send_reply(replies.ChannelModeIs(channel.name, channel.modes))
        # TODO: send creation time
      else:
        user = client.server.get_client(self.target)
        if user is not client:
          raise errors.UsersDontMatch

        client.send_reply(replies.UmodeIs(user.modes))
      return

    flags = []
    state = "+"
    for c in self.flags:
      if c in "+-":
        state = c
        continue
      flags.append(state + c)

    flags_with_args = itertools.zip_longest(flags, self.args, fillvalue=None)

    if util.is_channel_name(self.target):
      try:
        channel = client.server.get_channel(self.target)
      except errors.NoSuchNick:
        raise errors.NoSuchChannel(self.target)

      for flag, arg in flags_with_args:
        state, c = flag
        if state == "+":
          channel.set_mode(c, arg)
        elif state == "-":
          channel.unset_mode(c, arg)
      client.relay_to_channel(channel, Mode(channel.name, self.flags, *self.args))
      client.relay_to_self(Mode(channel.name, self.flags, *self.args))
    else:
      user = client.server.get_client(self.target)

      if user is not client:
        raise errors.UsersDontMatch

      for flag, arg in flags_with_args:
        state, c = flag
        if state == "+":
          user.set_mode(c, arg)
        elif state == "-":
          user.unset_mode(c, arg)
      client.relay_to_self(Mode(user.nickname, self.flags, *self.args))


class Who(Command):
  NAME = "WHO"
  MIN_ARITY = 0

  def __init__(self, target=None, only_opers=None, *args):
    self.target = target
    self.only_opers = only_opers

  @requires_registration
  def handle_for(self, client, prefix):
    who = []

    try:
      if util.is_channel_name(self.target):
        channel = client.server.get_channel(self.target)
        who = [(channel.name, user.client) for user in channel.users.values()]
      else:
        who = [(None, client.server.get_client(self.target))]
    except errors.NoSuchNick:
      pass

    for channel_name, user in who:
        client.send_reply(replies.WhoReply(channel_name, user, 0,
                                           client.server.name))

    client.send_reply(replies.EndOfWho())
