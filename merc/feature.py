import collections
import weakref


class FeatureMeta(type):
  def __new__(cls, names, bases, attrs):
    c = super().__new__(cls, names, bases, attrs)
    c.USER_COMMANDS = {}
    c.SERVER_COMMANDS = {}
    c.USER_MODES = {}
    c.CHANNEL_MODES = {}
    c.CAPABILITIES = {}
    c.HOOKS = collections.defaultdict(list)
    return c


class Feature(object, metaclass=FeatureMeta):
  def __init__(self, app):
    self.app = app

  @classmethod
  def install(cls, app):
    app.install_feature(cls(app))

  @classmethod
  def register_user_command(cls, command):
    cls.USER_COMMANDS[command.NAME] = command
    return command

  @classmethod
  def register_server_command(cls, command):
    cls.SERVER_COMMANDS[command.NAME] = command
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

  def run_hooks(self, name, app, *args, **kwargs):
    for hook in self.HOOKS[name]:
      hook(app, *args, **kwargs)
