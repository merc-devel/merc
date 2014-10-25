import operator

from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import util


MAX_MODES = 4


class ModeFeature(feature.Feature):
  NAME = __name__
  ISUPPORT = {
      "MODES": MAX_MODES
  }


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
  def _expand_flags(flags, args, modes):
    expanded = []

    args_iter = iter(args)

    op = "+"

    for i, c in enumerate(flags):
      if i > MAX_MODES:
        break

      if c in "+-":
        op = c
        continue

      arg = None

      try:
        mode = modes[c]
      except KeyError:
        raise errors.UnknownMode(c)

      if mode.TAKES_PARAM:
        try:
          arg = next(args_iter)
        except StopIteration:
          pass

      expanded.append((mode, op, arg))
    return expanded

  @staticmethod
  def _coalesce_flags(applied_flags):
    flags = ""
    args = []

    last_op = None

    for mode, op, arg in applied_flags:
      if op != last_op:
        flags += op
        last_op = op
      flags += mode.CHAR

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

      expanded_flags = self._expand_flags(self.flags, self.args,
                                          chan.modes)

      self.check_can_set_channel_modes(client, chan, expanded_flags)

      for mode, op, arg in expanded_flags:
        if (op == "+" and mode.set(client, arg)) or \
           (op == "-" and mode.unset(client, arg)):
          applied_flags.append((mode, op, arg))

      if applied_flags:
        flags, args = self._coalesce_flags(applied_flags)

        chan.broadcast(None, self.get_prefix(client),
                       Mode(chan.name, flags, *args))
    else:
      user = client.server.get_client(self.target)
      self.check_can_set_user_modes(client, user)

      try:
        expanded_flags = self._expand_flags(self.flags, self.args,
                                            user.modes)
      except errors.UnknownMode as e:
        raise errors.UmodeUnknownFlag(e.param)

      for mode, op, arg in expanded_flags:
        if (op == "+" and mode.set(user, arg)) or \
           (op == "-" and mode.unset(user, arg)):
          applied_flags.append((mode, op, arg))

      if applied_flags:
        flags, args = self._coalesce_flags(applied_flags)

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

  def check_can_set_channel_modes(self, client, channel, modes):
    for mode, op, arg in modes:
      if mode.TAKES_PARAM and arg is None:
        continue

      channel.check_is_operator(client)
      return

  def check_can_set_user_modes(self, client, user):
    if user is not client:
      raise errors.UsersDontMatch

  def get_prefix(self, client):
    return client.hostmask


@ModeFeature.register_command
class SAMode(_Mode):
  NAME = "SAMODE"
  MIN_ARITY = 2

  def check_can_set_channel_modes(self, client, channel, modes):
    client.check_is_irc_operator()

  def check_can_set_user_modes(self, client, user):
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
