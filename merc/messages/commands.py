import datetime
import functools
import itertools

from merc import channel
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
  REGISTRY = {}

  @classmethod
  def register(cls, type):
    cls.REGISTRY[type.NAME] = type
    return type

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


@Command.register
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


@Command.register
class Pong(Command):
  NAME = "PONG"
  MIN_ARITY = 2

  def __init__(self, server_name, value, *args):
    self.server_name = server_name
    self.value = value

  def as_params(self, client):
    return [self.server_name, self.value]


@Command.register
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


@Command.register
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


@Command.register
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


@Command.register
class Motd(Command):
  NAME = "MOTD"
  MIN_ARITY = 0

  @requires_registration
  def handle_for(self, client, prefix):
    client.send_reply(replies.MotdStart())

    for line in client.server.motd.split("\n"):
      client.send_reply(replies.Motd(line))

    client.send_reply(replies.EndOfMotd())


@Command.register
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
        try:
          channel = client.server.get_channel(target)
        except errors.NoSuchNick:
          continue

        if not client.is_in_channel(channel) and \
            channel.is_disallowing_external_messages:
          raise errors.CannotSendToChan(target)

        client.relay_to_channel(channel, Privmsg(channel.name, self.text))
      else:
        user = client.server.get_client(target)
        client.relay_to_client(user, Privmsg(user.nickname, self.text))


@Command.register
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
        try:
          channel = client.server.get_channel(target)
        except errors.NoSuchNick:
          continue

        if not client.is_in_channel(channel) and \
            channel.is_disallowing_external_messages:
          raise errors.CannotSendToChan(target)

        client.relay_to_channel(channel, Notice(channel.name, self.text))
      else:
        user = client.server.get_client(target)
        client.relay_to_client(user, Notice(user.nickname, self.text))


@Command.register
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

      if channel.topic is not None:
        client.on_message(client.hostmask, Topic(channel.name))

      client.on_message(client.hostmask, Names(channel.name))
      client.send_reply(replies.CreationTime(channel.name,
                                             channel.creation_time))

  def as_params(self, client):
    params = [",".join(self.channel_names)]
    if self.keys:
      params.append(",".join(self.keys))
    return params


@Command.register
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


@Command.register
class Names(Command):
  NAME = "NAMES"
  MIN_ARITY = 0

  def __init__(self, channel_names=None, *args):
    self.channel_names = channel_names.split(",") if channel_names is not None \
                                                  else None

  @requires_registration
  def handle_for(self, client, prefix):
    if self.channel_names is None:
      seen_nicknames = set()

      for chan in client.server.channels.values():
        if client.is_in_channel(chan):
          client.send_reply(replies.NameReply(
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
          client.send_reply(replies.NameReply("=", chan.name, channel_users))

      visible_users = []

      for user in client.server.clients.values():
        if user.is_invisible and user is not client:
          continue

        if user.normalized_nickname not in seen_nicknames:
          visible_users.append(channel.ChannelUser(None, user))

      if visible_users:
        client.send_reply(replies.NameReply("*", None, visible_users))

      client.send_reply(replies.EndOfNames(None))
    else:
      for channel_name in self.channel_names:
        try:
          chan = client.server.get_channel(channel_name)
        except errors.NoSuchNick:
          pass
        else:
          if client.can_see_channel(chan):
            channel_name = chan.name

            client.send_reply(replies.NameReply(
                "@" if client.is_in_channel(chan) else "*",
                chan.name,
                chan.get_visible_users_for(client)))
        client.send_reply(replies.EndOfNames(channel_name))

  def as_params(self, client):
    return [",".join(self.channel_names)]


@Command.register
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


@Command.register
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


@Command.register
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


@Command.register
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


@Command.register
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
          chan = client.server.get_channel(self.target)
        except errors.NoSuchNick:
          raise errors.NoSuchChannel(self.target)

        client.send_reply(replies.ChannelModeIs(chan.name, chan.modes))
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

    args_iter = iter(self.args)

    applied_flags = []

    if util.is_channel_name(self.target):
      try:
        chan = client.server.get_channel(self.target)
      except errors.NoSuchNick:
        raise errors.NoSuchChannel(self.target)

      expanded_args = []

      for flag in flags:
        state, c = flag
        arg = None

        if c in channel.Channel.MODES_WITH_PARAMS:
          try:
            arg = next(args_iter)
          except StopIteration:
            pass
        expanded_args.append(arg)

      for flag, arg in zip(flags, expanded_args):
        state, c = flag
        if state == "+":
          if chan.set_mode(client, c, arg):
            applied_flags.append((flag, arg))
        elif state == "-":
          if chan.unset_mode(client, c, arg):
            applied_flags.append((flag, arg))

      if applied_flags:
        flags, args = self._coalesce_flags(applied_flags)
        client.relay_to_channel(chan, Mode(chan.name, flags, *args))
        client.relay_to_self(Mode(chan.name, flags, *args))
    else:
      user = client.server.get_client(self.target)

      if user is not client:
        raise errors.UsersDontMatch

      for flag in flags:
        # TODO: do users have parametrized flags?
        state, c = flag
        if state == "+":
          if user.set_mode(self, c, None):
            applied_flags.append((flag, None))
        elif state == "-":
          if user.unset_mode(self, c, None):
            applied_flags.append((flag, None))

      if applied_flags:
        flags, args = self._coalesce_flags(applied_flags)
        client.relay_to_self(Mode(user.nickname, flags, *args))

  def _coalesce_flags(self, applied_flags):
    flags = ""
    args = []

    last_state = None
    for flag, arg in applied_flags:
      state, c = flag
      if state != last_state:
        flags += state
        last_state = state
      flags += c

      if arg is not None:
        args.append(arg)

    return flags, args


@Command.register
class Who(Command):
  NAME = "WHO"
  MIN_ARITY = 1

  def __init__(self, target, only_opers=None, *args):
    self.target = target
    self.only_opers = only_opers

  @requires_registration
  def handle_for(self, client, prefix):
    who = []

    try:
      if util.is_channel_name(self.target):
        channel = client.server.get_channel(self.target)

        if client.can_see_channel(channel):
          who = [(channel.name, user.client)
                 for user in channel.get_visible_users_for(client)]
      else:
        for user in client.server.query_clients(self.target):
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
        client.send_reply(replies.WhoReply(channel_name, user, 0,
                                           client.server.name))

    client.send_reply(replies.EndOfWho())


@Command.register
class List(Command):
  NAME = "LIST"
  MIN_ARITY = 0

  def __init__(self, target=None, *args):
    self.target = target

  @requires_registration
  def handle_for(self, client, prefix):
    client.send_reply(replies.ListStart())

    try:
      if self.target is not None:
        channels = [client.server.get_channel(self.target)]
      else:
        channels = (channel for channel in client.server.channels.values())

      for channel in channels:
        if not client.can_see_channel(channel):
          continue

        if client.is_in_channel(channel):
          num_visible = len(channel.users)
        else:
          num_visible = sum(not user.client.is_invisible
                            for user in channel.users.values())

        client.send_reply(replies.List(
            channel.name, num_visible,
            channel.topic is not None and channel.topic.text or ""))
    finally:
      client.send_reply(replies.ListEnd())
