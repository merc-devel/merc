import collections
import functools
import logging
import imp
import importlib

from merc import features

logger = logging.getLogger(__name__)


class FeatureMeta(type):
  def __new__(cls, names, bases, attrs):
    c = super().__new__(cls, names, bases, attrs)
    c.CONFIG_CHECKERS = []
    c.USER_COMMANDS = {}
    c.SERVER_COMMANDS = {}
    c.SERVER_NUMERIC_COMMAND = None
    c.USER_MODES = {}
    c.CHANNEL_MODES = {}
    c.USER_CAPABILITIES = {}
    c.SERVER_CAPABILITIES = {}
    c.HOOKS = collections.defaultdict(set)
    return c


class Feature(object, metaclass=FeatureMeta):
  CONFIG_SECTION = None

  def __init__(self, app):
    self.app = app

  @classmethod
  def install(cls, app):
    app.features.install(cls(app))

  def uninstall(self, app):
    app.features.uninstall(self)

  @classmethod
  def register_config_checker(cls, checker):
    cls.CONFIG_CHECKERS.append(checker)
    return checker

  @classmethod
  def register_user_command(cls, command):
    cls.USER_COMMANDS[command.NAME] = command
    return command

  @classmethod
  def register_server_command(cls, command):
    cls.SERVER_COMMANDS[command.NAME] = command
    return command

  @classmethod
  def register_server_numeric_command(cls, command):
    if cls.SERVER_NUMERIC_COMMAND is not None:
      raise ValueError("cannot register two numeric command handlers")

    cls.SERVER_NUMERIC_COMMAND = command
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
  def register_user_capability(cls, capability):
    cls.USER_CAPABILITIES[capability.NAME] = capability
    return capability

  @classmethod
  def register_server_capability(cls, capability):
    cls.SERVER_CAPABILITIES[capability.NAME] = capability
    return capability

  @classmethod
  def hook(cls, name):
    def _wrapper(f):
      cls.HOOKS[name].add(f)
      return f
    return _wrapper


class FeatureLoader(object):
  def __init__(self, app):
    self.features = {}

    self.user_commands = {}
    self.server_commands = {}
    self.server_numeric_command = None
    self.hooks = collections.defaultdict(set)

    self.app = app

  def all(self):
    yield from self.features.values()

  def get(self, feature_factory):
    return self.features[feature_factory.NAME]

  def load(self, name):
    if name[0] == ".":
      name = features.__name__ + name

    try:
      module = imp.reload(importlib.import_module(name))

      try:
        install = module.install
      except AttributeError:
        logger.critical("{} does not name a merc feature!".format(name))
        return

      install(self.app)
    except Exception:
      logger.critical("{} could not be loaded.".format(name), exc_info=True)
      return

  def install(self, feature):
    self.features[feature.NAME] = feature

    self.user_commands.update(feature.USER_COMMANDS)
    self.server_commands.update(feature.SERVER_COMMANDS)
    self.server_numeric_command = feature.SERVER_NUMERIC_COMMAND
    for name, hooks in feature.HOOKS.items():
      self.hooks[name].update(hooks)

    logger.info("{} installed.".format(feature.NAME))

  def unload(self, name):
    if name[0] == ".":
      name = features.__name__ + name

    try:
      feature = self.features[name]
    except KeyError:
      logging.warn("{} could not be loaded as it was not loaded.".format(name))
    else:
      feature.uninstall(self.app)

  def uninstall(self, feature):
    del self.features[feature.NAME]

    for name in feature.USER_COMMANDS:
      del self.user_commands[name]

    for name in feature.SERVER_COMMANDS:
      del self.server_commands[name]

    if feature.SERVER_NUMERIC_COMMAND is not None:
      self.server_numeric_command = None

    for name, hooks in feature.HOOKS.items():
      self.hooks[name].difference_update(hooks)

    logger.info("{} uninstalled.".format(feature.NAME))

  def unload_all(self):
    for feature_name in list(self.features.keys()):
      self.unload(feature_name)

  def check_config(self, config):
    for feature in self.all():
      if feature.CONFIG_CHECKERS:
        key = feature.CONFIG_SECTION or feature.NAME
        section = config.get(key)

        for checker in feature.CONFIG_CHECKERS:
          section = checker(section)

        config[key] = section

  def get_user_command(self, name):
    return self.user_commands[name]

  def get_server_command(self, name):
    if name.isnumeric() and self.server_numeric_command is not None:
      return functools.partial(self.server_numeric_command, name)
    return self.server_commands[name]

  def get_hooks(self, name):
    return self.hooks.get(name, [])

  def get_config_section(self, name):
    feature = self.features[name]
    if feature.CONFIG_SECTION:
      name = feature.CONFIG_SECTION

    return self.app.config.get(name)
