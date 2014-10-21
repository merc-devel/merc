from merc import channel
from merc import errors
from merc import message
from merc import util


class UmodeIs(message.Reply):
  NAME = "221"

  def __init__(self, modes):
    self.modes = modes

  def as_reply_params(self, client):
    flags, args = util.show_modes(self.modes)
    return [flags] + args


class ChannelModeIs(message.Reply):
  NAME = "324"

  def __init__(self, channel_name, modes):
    self.channel_name = channel_name
    self.modes = modes

  def as_reply_params(self, client):
    flags, args = util.show_modes(self.modes)
    return [self.channel_name, flags] + args


@message.Command.register
class Mode(message.Command):
  NAME = "MODE"
  MIN_ARITY = 1

  def __init__(self, target, flags=None, *args):
    self.target = target
    self.flags = flags
    self.args = args

  def as_params(self, client):
    return [self.target, self.flags] + list(self.args)

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    if self.flags is None:
      if util.is_channel_name(self.target):
        try:
          chan = client.server.get_channel(self.target)
        except errors.NoSuchNick:
          raise errors.NoSuchChannel(self.target)

        client.send_reply(ChannelModeIs(chan.name, chan.modes))
        # TODO: send creation time
      else:
        user = client.server.get_client(self.target)
        if user is not client:
          raise errors.UsersDontMatch

        client.send_reply(UmodeIs(user.modes))
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