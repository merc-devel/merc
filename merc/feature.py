import collections
import weakref


class FeatureMeta(type):
  def __new__(cls, names, bases, attrs):
    c = super().__new__(cls, names, bases, attrs)
    c.COMMANDS = {}
    c.USER_MODES = {}
    c.CHANNEL_MODES = {}
    c.HOOKS = collections.defaultdict(list)
    return c


class ChannelFeatureContext(object):
  def __init__(self, feature, channel):
    self.locals = {}
    self.modes = {mode.CHAR: mode(channel)
                  for mode in feature.CHANNEL_MODES.values()}


class UserFeatureContext(object):
  def __init__(self, feature, user):
    self.locals = {}
    self.modes = {mode.CHAR: mode(user)
                  for mode in feature.USER_MODES.values()}


class Feature(object, metaclass=FeatureMeta):
  ISUPPORT = {}

  def __init__(self, server):
    self.server = server

    self.user_contexts = weakref.WeakKeyDictionary()
    self.channel_contexts = weakref.WeakKeyDictionary()

  def _get_user_context(self, user):
    if user not in self.user_contexts:
      self.user_contexts[user] = UserFeatureContext(self, user)
    return self.user_contexts[user]

  def _get_channel_context(self, channel):
    if channel not in self.channel_contexts:
      self.channel_contexts[channel] = ChannelFeatureContext(self, channel)
    return self.channel_contexts[channel]

  def get_user_locals(self, user):
    return self._get_user_context(user).locals

  def get_channel_locals(self, channel):
    return self._get_channel_context(channel).locals

  def get_user_modes(self, user):
    return self._get_user_context(user).modes

  def get_channel_modes(self, channel):
    return self._get_channel_context(channel).modes

  @classmethod
  def register_command(cls, command):
    cls.COMMANDS[command.NAME] = command
    return command

  @classmethod
  def register_channel_mode(cls, mode):
    cls.CHANNEL_MODES[mode.CHAR] = mode
    return mode

  @classmethod
  def register_user_mode(cls, mode):
    cls.USER_MODES[mode.CHAR] = mode
    return mode

  @classmethod
  def hook(cls, name):
    def _wrapper(f):
      cls.HOOKS[name].append(f)
      return f
    return _wrapper

  def run_hooks(self, name, *args, **kwargs):
    for hook in self.HOOKS[name]:
      hook(*args, **kwargs)
