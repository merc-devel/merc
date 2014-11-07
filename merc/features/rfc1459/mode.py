import operator

from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import mode


MAX_MODES = 4


class ModeFeature(feature.Feature):
  NAME = __name__

install = ModeFeature.install


def show_modes(target, modes):
  flags = []
  args = []

  for k, mode_factory in sorted(modes.items(), key=operator.itemgetter(0)):
    m = mode_factory(target)
    value = m.get()

    if value:
      flags.append(m.CHAR)

      if value is not True:
        args.append(str(value))

  return "+" + "".join(flags), args


class UmodeIs(message.Reply):
  NAME = "221"
  MIN_ARITY = 1

  def __init__(self, flags, *args):
    self.flags = flags
    self.args = list(args)

  def as_reply_params(self):
    return [self.flags] + self.args


class ChannelModeIs(message.Reply):
  NAME = "324"
  MIN_ARITY = 2

  def __init__(self, channel_name, flags, *args):
    self.channel_name = channel_name
    self.flags = flags
    self.args = list(args)

  def as_reply_params(self):
    return [self.channel_name, self.flags] + self.args


class CreationTime(message.Reply):
  NAME = "329"
  MIN_ARITY = 2

  def __init__(self, channel_name, time, *args):
    self.channel_name = channel_name
    self.time = time

  def as_reply_params(self):
    return [self.channel_name, self.time]


class _Mode(message.Command):
  def __init__(self, target, flags=None, *args):
    self.target = target
    self.flags = flags
    self.args = list(args)

  def as_command_params(self):
    return [self.target, self.flags] + list(self.args)

  @staticmethod
  def _expand_modes(flags, args, modes):
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
        m = modes[c]
      except KeyError:
        raise errors.UnknownMode(c)

      if m.TAKES_PARAM:
        try:
          arg = next(args_iter)
        except StopIteration:
          pass

      expanded.append((m, op, arg))
    return expanded

  @staticmethod
  def _coalesce_modes(applied):
    flags = ""
    args = []

    last_op = None

    for m, op, arg in applied:
      if op != last_op:
        flags += op
        last_op = op
      flags += m.CHAR

      if arg is not None:
        args.append(str(arg))

    return flags, args

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    applied = []

    if channel.Channel.is_channel_name(self.target):
      try:
        chan = app.channels.get(self.target)
      except errors.NoSuchNick:
        raise errors.NoSuchChannel(self.target)

      expanded = self._expand_modes(self.flags, self.args,
                                    app.channels.modes)

      self.check_can_set_channel_modes(app, user, chan, expanded)

      for mode_factory, op, arg in expanded:
        m = mode_factory(chan)

        if (op == "+" and m.set(app, user, arg)) or \
           (op == "-" and m.unset(app, user, arg)):
          applied.append((mode_factory, op, arg))

      if applied:
        flags, args = self._coalesce_modes(applied)

        chan.broadcast(None, self.get_prefix(app, user),
                       Mode(chan.name, flags, *args))
    else:
      target = app.users.get(self.target)

      try:
        expanded = self._expand_modes(self.flags, self.args,
                                      app.users.modes)
      except errors.UnknownMode as e:
        raise errors.UmodeUnknownFlag(e.param)

      self.check_can_set_user_modes(app, user, target, expanded)

      for mode_factory, op, arg in expanded:
        m = mode_factory(target)

        if (op == "+" and m.set(app, target, arg)) or \
           (op == "-" and m.unset(app, target, arg)):
          applied.append((mode_factory, op, arg))

      if applied:
        flags, args = self._coalesce_modes(applied)
        target.send(self.get_prefix(app, user),
                    Mode(target.nickname, flags, *args))


@ModeFeature.register_user_command
class Mode(_Mode):
  NAME = "MODE"
  MIN_ARITY = 1

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    if self.flags is None:
      if channel.Channel.is_channel_name(self.target):
        try:
          chan = app.channels.get(self.target)
        except errors.NoSuchNick:
          raise errors.NoSuchChannel(self.target)

        flags, args = show_modes(chan, app.channels.modes)
        user.send_reply(ChannelModeIs(chan.name, flags, *args))
      else:
        target = app.users.get(self.target)
        if target is not user:
          raise errors.UsersDontMatch

        flags, args = show_modes(target, app.users.modes)
        user.send_reply(UmodeIs(flags, *args))
    else:
        super().handle_for(app, user, prefix)

  def check_can_set_channel_modes(self, app, user, channel, modes):
    for m, op, arg in modes:
      if issubclass(m, mode.ListMode) and arg is None:
        continue

      m(channel).check(app, user, arg)
      return

  def check_can_set_user_modes(self, app, user, target, modes):
    for m, op, arg in modes:
      if issubclass(m, mode.ListMode) and arg is None:
        continue

      m(target).check(app, user, arg)
      return

  def get_prefix(self, app, user):
    return user.hostmask


@ModeFeature.register_user_command
class SAMode(_Mode):
  NAME = "SAMODE"
  MIN_ARITY = 2

  def check_can_set_channel_modes(self, app, user, channel, modes):
    user.check_is_irc_operator()

  def check_can_set_user_modes(self, app, user, target, modes):
    user.check_is_irc_operator()

  def get_prefix(self, app, user):
    return app.server.name


@ModeFeature.hook("user.welcome")
def send_modes_on_welcome(app, user):
  flags, args = show_modes(user, app.users.modes)
  if flags != "+":
    user.send(user.prefix, Mode(user.nickname, flags, *args))


@ModeFeature.hook("channel.join")
def send_timestamp_on_join(app, user, target, channel):
  target.send_reply(CreationTime(channel.name,
                                 str(int(channel.creation_time.timestamp()))))


@ModeFeature.hook("channel.join_new")
def send_channel_modes_on_new_join(app, user, target, channel):
  flags, args = show_modes(channel, app.channels.modes)
  target.send_reply(Mode(channel.name, flags, *args))


@ModeFeature.hook("user.mode_change")
def send_mode_on_user_mode_change(app, user, applied):
  flags, args = Mode._coalesce_modes(applied)
  user.send(user.prefix, Mode(user.nickname, flags, *args))


@ModeFeature.hook("server.isupport.modify")
def modify_isupport(app, isupport):
  list_modes = set()
  param_modes = set()
  set_with_param_modes = set()
  flag_modes = set()

  for m in app.channels.modes.values():
    if issubclass(m, mode.ListMode):
      list_modes.add(m.CHAR)
    elif issubclass(m, mode.ParamMode):
      param_modes.add(m.CHAR)
    elif issubclass(m, mode.SetWithParamMode):
      set_with_param_modes.add(m.CHAR)
    elif issubclass(m, mode.FlagMode):
      flag_modes.add(m.CHAR)

  isupport["MODES"] = MAX_MODES
  isupport["CHANMODES"] = ",".join(["".join(sorted(list_modes)),
                                    "".join(sorted(param_modes)),
                                    "".join(sorted(set_with_param_modes)),
                                    "".join(sorted(flag_modes))])
  isupport["MAXLIST"] = "{}:{}".format("".join(sorted(list_modes)),
                                       mode.ListMode.MAX_ITEMS)
