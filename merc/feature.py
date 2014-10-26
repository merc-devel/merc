import collections
import weakref


class FeatureMeta(type):
  def __new__(cls, names, bases, attrs):
    c = super().__new__(cls, names, bases, attrs)
    c.COMMANDS = {}
    c.USER_MODES = {}
    c.CHANNEL_MODES = {}
    c.CAPABILITIES = {}
    c.HOOKS = collections.defaultdict(list)
    return c


class Feature(object, metaclass=FeatureMeta):
  def __init__(self, server):
    self.server = server

  @classmethod
  def install(cls, server):
    server.install_feature(cls(server))

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
  def register_capability(cls, capability):
    cls.CAPABILITIES[capability.NAME] = capability
    return capability

  @classmethod
  def hook(cls, name):
    def _wrapper(f):
      cls.HOOKS[name].append(f)
      return f
    return _wrapper

  def run_hooks(self, name, *args, **kwargs):
    for hook in self.HOOKS[name]:
      hook(*args, **kwargs)
