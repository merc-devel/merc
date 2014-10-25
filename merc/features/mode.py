import operator

from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import mode
from merc import util


MAX_MODES = 4


class ModeFeature(feature.Feature):
  NAME = __name__

  @property
  def isupport(self):
    list_modes = set()
    param_modes = set()
    set_with_param_modes = set()
    flag_modes = set()

    for m in self.server.channel_modes.values():
      if issubclass(m, mode.ListMode):
        list_modes.add(m.CHAR)
      elif issubclass(m, mode.ParamMode):
        param_modes.add(m.CHAR)
      elif issubclass(m, mode.SetWithParamMode):
        set_with_param_modes.add(m.CHAR)
      elif issubclass(m, mode.FlagMode):
        flag_modes.add(m.CHAR)

    return {
        "MODES": MAX_MODES,
        "CHANMODES": ",".join(["".join(sorted(list_modes)),
                               "".join(sorted(param_modes)),
                               "".join(sorted(set_with_param_modes)),
                               "".join(sorted(flag_modes))])
    }


install = ModeFeature


def show_modes(target, modes):
  flags = []
  args = []

  for k, mode_factory in sorted(modes.items(), key=operator.itemgetter(0)):
    mode = mode_factory(target)
    value = mode.get()

    if value:
      flags.append(mode.CHAR)

      if value is not True:
        args.append(value)

  return "+" + "".join(flags), args


class UmodeIs(message.Reply):
  NAME = "221"

  def as_reply_params(self, client):
    flags, args = show_modes(client, client.server.user_modes)
    return [flags] + args


class ChannelModeIs(message.Reply):
  NAME = "324"

  def __init__(self, channel):
    self.channel = channel

  def as_reply_params(self, client):
    flags, args = show_modes(self.channel, client.server.channel_modes)
    return [self.channel.name, flags] + args


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

    for c in flags:
      if len(expanded) > MAX_MODES:
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
                                          client.server.channel_modes)

      self.check_can_set_channel_modes(client, chan, expanded_flags)

      for mode_factory, op, arg in expanded_flags:
        mode = mode_factory(chan)

        if (op == "+" and mode.set(client, arg)) or \
           (op == "-" and mode.unset(client, arg)):
          applied_flags.append((mode_factory, op, arg))

      if applied_flags:
        flags, args = self._coalesce_flags(applied_flags)

        chan.broadcast(None, self.get_prefix(client),
                       Mode(chan.name, flags, *args))
    else:
      user = client.server.get_client(self.target)
      self.check_can_set_user_modes(client, user)

      try:
        expanded_flags = self._expand_flags(self.flags, self.args,
                                            client.server.user_modes)
      except errors.UnknownMode as e:
        raise errors.UmodeUnknownFlag(e.param)

      for mode_factory, op, arg in expanded_flags:
        mode = mode_factory(user)

        if (op == "+" and mode.set(user, arg)) or \
           (op == "-" and mode.unset(user, arg)):
          applied_flags.append((mode_factory, op, arg))

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

        client.send_reply(ChannelModeIs(chan))
      else:
        user = client.server.get_client(self.target)
        if user is not client:
          raise errors.UsersDontMatch

        client.send_reply(UmodeIs())
    else:
        super().handle_for(client, prefix)

  def check_can_set_channel_modes(self, client, channel, modes):
    for m, op, arg in modes:
      if isinstance(m, mode.ListMode) and arg is None:
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
    flags, args = show_modes(client, client.server.user_modes)
    client.relay_to_self(Mode(client.nickname, flags, *args))


@ModeFeature.hook("after_join_channel")
def send_timestamp_on_join(client, user, channel):
  user.send_reply(CreationTime(channel.name, channel.creation_time))


@ModeFeature.hook("after_join_new_channel")
def send_channel_modes_on_new_join(client, user, channel):
  flags, args = show_modes(channel, client.server.channel_modes)
  user.send_reply(Mode(channel.name, flags, *args))


@ModeFeature.hook("user_mode_change")
def send_mode_on_user_mode_change(client, modes):
  return
  flags, args = show_modes(modes)
  client.relay_to_self(Mode(client.nickname, flags, *args))
