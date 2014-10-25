import operator

from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import util


class ModeFeature(feature.Feature):
  NAME = __name__


install = ModeFeature


def show_modes(modes):
  flags = []
  args = []

  for k, mode in sorted(modes.items(), key=operator.itemgetter(0)):
    value = mode.get()

    if mode.TAKES_PARAM:
      if value is not None:
        flags.append(mode.CHAR)
        args.append(value)
    else:
      if value:
        flags.append(mode.CHAR)

  return "+" + "".join(flags), args


class UmodeIs(message.Reply):
  NAME = "221"

  def __init__(self, modes):
    self.modes = modes

  def as_reply_params(self, client):
    flags, args = show_modes(self.modes)
    return [flags] + args


class ChannelModeIs(message.Reply):
  NAME = "324"

  def __init__(self, channel_name, modes):
    self.channel_name = channel_name
    self.modes = modes

  def as_reply_params(self, client):
    flags, args = show_modes(self.modes)
    return [self.channel_name, flags] + args


class CreationTime(message.Reply):
  NAME = "329"

  def __init__(self, channel_name, time):
    self.channel_name = channel_name
    self.time = time

  def as_reply_params(self, client):
    return [self.channel_name, str(int(self.time.timestamp()))]


class _Mode(message.Command):
  def __init__(self, target, flags=None, *args):
    self.target = target
    self.flags = flags
    self.args = args

  def as_params(self, client):
    return [self.target, self.flags] + list(self.args)

  @staticmethod
  def _parse_flags(flags, args, modes_with_params=None):
    if modes_with_params is None:
      modes_with_params = set()

    args_iter = iter(args)

    state = "+"

    for c in flags:
      if c in "+-":
        state = c
        continue

      arg = None

      if c in modes_with_params:
        try:
          arg = next(args_iter)
        except StopIteration:
          pass

      yield state + c, arg

  @staticmethod
  def _emit_flags(applied_flags):
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

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    applied_flags = []

    if channel.Channel.is_valid_name(self.target):
      try:
        chan = client.server.get_channel(self.target)
      except errors.NoSuchNick:
        raise errors.NoSuchChannel(self.target)

      self.check_can_set_channel_flags(client, chan)

      for flag, arg in self._parse_flags(
          self.flags, self.args,
          {mode.CHAR for mode in client.server.channel_modes.values()
                     if mode.TAKES_PARAM}):
        state, c = flag

        if state == "+":
          if chan.set_mode(client, c, arg):
            applied_flags.append((flag, arg))
        elif state == "-":
          if chan.unset_mode(client, c, arg):
            applied_flags.append((flag, arg))

      if applied_flags:
        flags, args = self._emit_flags(applied_flags)

        chan.broadcast(None, self.get_prefix(client),
                       Mode(chan.name, flags, *args))
    else:
      user = client.server.get_client(self.target)
      self.check_can_set_user_flags(client, user)

      for flag, arg in self._parse_flags(
          self.flags, self.args,
          {mode.CHAR for mode in client.server.user_modes.values()
                     if mode.TAKES_PARAM}):
        state, c = flag

        if state == "+":
          if user.set_mode(self, c, arg):
            applied_flags.append((flag, arg))
        elif state == "-":
          if user.unset_mode(self, c, arg):
            applied_flags.append((flag, arg))

      if applied_flags:
        flags, args = self._emit_flags(applied_flags)

        msg_prefix = self.get_prefix(client)
        user.send(self.get_prefix(client),
                  Mode(user.nickname, flags, *args))


@ModeFeature.register_command
class Mode(_Mode):
  NAME = "MODE"
  MIN_ARITY = 1

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    if self.flags is None:
      if channel.Channel.is_valid_name(self.target):
        try:
          chan = client.server.get_channel(self.target)
        except errors.NoSuchNick:
          raise errors.NoSuchChannel(self.target)

        client.send_reply(ChannelModeIs(chan.name, chan.modes))
      else:
        user = client.server.get_client(self.target)
        if user is not client:
          raise errors.UsersDontMatch

        client.send_reply(UmodeIs(user.modes))
    else:
        super().handle_for(client, prefix)

  def check_can_set_channel_flags(self, client, channel):
    channel.check_is_operator(client)

  def check_can_set_user_flags(self, client, user):
    if user is not client:
      raise errors.UsersDontMatch

  def get_prefix(self, client):
    return client.hostmask


@ModeFeature.register_command
class SAMode(_Mode):
  NAME = "SAMODE"
  MIN_ARITY = 2

  def check_can_set_channel_flags(self, client, channel):
    client.check_is_irc_operator()

  def check_can_set_user_flags(self, client, user):
    client.check_is_irc_operator()

  def get_prefix(self, client):
    return client.server.name


@ModeFeature.hook("after_welcome")
def send_modes_on_welcome(client):
  if client.modes:
    flags, args = show_modes(client.modes)
    client.relay_to_self(Mode(client.nickname, flags, *args))


@ModeFeature.hook("after_join_channel")
def send_timestamp_on_join(client, user, channel):
  user.send_reply(CreationTime(channel.name, channel.creation_time))


@ModeFeature.hook("after_join_new_channel")
def send_channel_modes_on_new_join(client, user, channel):
  flags, args = show_modes(channel.modes)
  user.send_reply(Mode(channel.name, flags, *args))


@ModeFeature.hook("user_mode_change")
def send_mode_on_user_mode_change(client, modes):
  flags, args = show_modes(modes)
  client.relay_to_self(Mode(client.nickname, flags, *args))
