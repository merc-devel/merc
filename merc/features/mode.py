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


class _Mode(message.Command):
  def __init__(self, target, flags=None, *args):
    self.target = target
    self.flags = flags
    self.args = args

  def as_params(self, client):
    return [self.target, self.flags] + list(self.args)

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    flags = []
    state = "+"
    for c in self.flags:
      if c in "+-":
        state = c
        continue
      flags.append(state + c)

    args_iter = iter(self.args)

    applied_flags = []

    if channel.Channel.is_valid_name(self.target):
      try:
        chan = client.server.get_channel(self.target)
      except errors.NoSuchNick:
        raise errors.NoSuchChannel(self.target)

      self.check_can_set_chanenl_flags(client, chan)

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

        chan.broadcast(None, self.get_prefix(client),
                       Mode(chan.name, flags, *args))
    else:
      user = client.server.get_client(self.target)
      self.check_can_set_user_flags(client, user)

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

        msg_prefix = self.get_prefix(client)
        user.send(self.get_prefix(client),
                  Mode(user.nickname, flags, *args))

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


@message.Command.register
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
        # TODO: send creation time
      else:
        user = client.server.get_client(self.target)
        if user is not client:
          raise errors.UsersDontMatch

        client.send_reply(UmodeIs(user.modes))
    else:
        super().handle_for(client, prefix)

  def check_can_set_chanenl_flags(self, client, channel):
    channel.check_is_operator(client)

  def check_can_set_user_flags(self, client, user):
    if user is not client:
      raise errors.UsersDontMatch

  def get_prefix(self, client):
    return client.hostmask


@message.Command.register
class SAMode(_Mode):
  NAME = "SAMODE"
  MIN_ARITY = 2

  def check_can_set_chanenl_flags(self, client, channel):
    client.check_is_irc_operator()

  def check_can_set_user_flags(self, client, user):
    client.check_is_irc_operator()

  def get_prefix(self, client):
    return client.server.name
