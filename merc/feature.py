import collections
import weakref
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
    c.USER_MODES = {}
    c.CHANNEL_MODES = {}
    c.USER_CAPABILITIES = {}
    c.SERVER_CAPABILITIES = {}
    c.HOOKS = collections.defaultdict(list)
    return c


class Feature(object, metaclass=FeatureMeta):
  CONFIG_SECTION = None

  def __init__(self, app):
    self.app = app

  @classmethod
  def install(cls, app):
    app.features.install(cls(app))

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
      cls.HOOKS[name].append(f)
      return f
    return _wrapper

  def run_hooks(self, name, app, *args, **kwargs):
    for hook in self.HOOKS[name]:
      hook(app, *args, **kwargs)


class FeatureLoader(object):
  def __init__(self, app):
    self.features = {}
    self.app = app

  def all(self):
    yield from self.features.values()

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
    logger.info("{} installed.".format(feature.NAME))

  def unload(self, name):
    if name[0] == ".":
      name = features.__name__ + name

    try:
      feature = self.features[name]
    except KeyError:
      logging.warn("{} could not be loaded as it was not loaded.".format(name))
    else:
      del self.features[name]
      logger.info("{} unloaded.".format(feature.NAME))

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
    for feature in self.all():
      if name in feature.USER_COMMANDS:
        return feature.USER_COMMANDS[name]

    raise KeyError(name)

  def get_server_command(self, name):
    for feature in self.all():
      if name in feature.SERVER_COMMANDS:
        return feature.SERVER_COMMANDS[name]

    raise KeyError(name)

  def get_config_section(self, name):
    feature = self.features[name]
    if feature.CONFIG_SECTION:
      name = feature.CONFIG_SECTION

    return self.app.config.get(name)