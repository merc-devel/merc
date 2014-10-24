import collections


class FeatureMeta(type):
  def __new__(cls, names, bases, attrs):
    c = super().__new__(cls, names, bases, attrs)
    c.COMMANDS = {}
    c.MODES = {}
    c.HOOKS = collections.defaultdict(list)
    return c


class Feature(object, metaclass=FeatureMeta):
  ISUPPORT = {}

  def __init__(self, server):
    self.server = server

  @classmethod
  def register_command(cls, command):
    cls.COMMANDS[command.NAME] = command
    return command

  @classmethod
  def register_channel_mode(cls, mode):
    cls.MODES[mode.CHAR] = mode
    return mode

  @classmethod
  def hook(cls, name):
    def _wrapper(f):
      cls.HOOKS[name].append(f)
      return f
    return _wrapper
