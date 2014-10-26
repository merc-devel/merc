import aiodns
import argparse
import asyncio
import datetime
import imp
import importlib
import logging
import operator
import regex
import signal
import ssl
import yaml

import passlib.context

import merc

from merc import channel
from merc import errors
from merc import features
from merc import message
from merc import net
from merc import user
from merc import util

logger = logging.getLogger(__name__)


class Server(object):
  def __init__(self, config_filename, loop=None):
    if loop is None:
      loop = asyncio.get_event_loop()

    self.loop = loop

    self.features = {}

    self.config_filename = config_filename
    self.rehash()

    self.resolver = aiodns.DNSResolver(loop=loop)
    self.crypt_context = passlib.context.CryptContext(
        schemes=self.config["crypto"]["hash_schemes"])

    self.creation_time = datetime.datetime.now()

    self.users = {}
    self.channels = {}

    self.register_signal_handlers()

  def rehash(self):
    self._unload_all_features()
    with open(self.config_filename, "r") as f:
      self.config = yaml.load(f)
    self._load_configured_features()

  @property
  def name(self):
    return self.config["server_name"]

  @property
  def version(self):
    return util.get_version()

  @property
  def network_name(self):
    return self.config["network_name"]

  @property
  def motd(self):
    return self.config["motd"]

  @property
  def admin_location(self):
    return self.config["admin"]["location"]

  @property
  def admin_location_fine(self):
    return self.config["admin"]["location_fine"]

  @property
  def admin_name(self):
    return self.config["admin"]["name"]

  @property
  def admin_email(self):
    return self.config["admin"]["email"]

  def register_signal_handlers(self):
    signal.signal(signal.SIGHUP, lambda signum, frame: self.rehash())

  @property
  def isupport(self):
    isupport = {
      "CHANTYPES": "".join(channel.Channel.CHANNEL_CHARS),
      "NETWORK": self.network_name,
      "CASEMAPPING": "unicode",
      "CHARSET": "utf-8"
    }

    for feature in self.features.values():
      isupport.update(feature.isupport)
    return isupport

  def new_user(self, transport):
    u = user.User(self, transport)

    logger.info("Accepted connection from {}".format(
        transport.get_extra_info("peername")))

    self.run_hooks("after_new_user", u)
    return u

  def register_user(self, user):
    if user.normalized_nickname in self.users:
      host, *_ = user.transport.get_extra_info("peername")
      raise errors.Error("Closing Link: {} (Overridden)".format(host))

    self.users[user.normalized_nickname] = user
    user.is_registered = True

    self.run_hooks("after_register", user)

  def rename_user(self, user, new_nickname):
    normalized_new_nickname = util.to_irc_lower(new_nickname)

    if normalized_new_nickname in self.users and \
       self.users[normalized_new_nickname] is not user:
      raise errors.NicknameInUse(new_nickname)

    if user.is_registered:
      del self.users[user.normalized_nickname]

    user.nickname = new_nickname

    if user.is_registered:
      self.users[user.normalized_nickname] = user

  def remove_user(self, user):
    self.run_hooks("before_remove_user", user)
    if user.is_registered:
      del self.users[user.normalized_nickname]
    user.on_close()
    self.run_hooks("after_remove_user", user)

  def get_user(self, name):
    try:
      return self.users[util.to_irc_lower(name)]
    except KeyError:
      raise errors.NoSuchNick(name)

  def get_channel(self, name):
    try:
      return self.channels[util.to_irc_lower(name)]
    except KeyError:
      raise errors.NoSuchNick(name)

  def new_channel(self, name):
    c = channel.Channel(self, name)
    self.channels[c.normalized_name] = c
    return c

  def remove_channel(self, channel):
    del self.channels[channel.normalized_name]
    self.run_hooks("after_remove_channel", channel)

  def part_channel(self, user, name):
    channel = self.get_channel(name)
    channel.part(user)

    if not channel.users:
      self.remove_channel(channel)

    return channel

  def query_users(self, pattern):
    if "!" not in pattern:
      pattern += "!*@*"

    return (user for user in self.users.values()
                   if user.hostmask_matches(pattern))

  def run_hooks(self, hook_name, *args, **kwargs):
    for feature in self.features.values():
      feature.run_hooks(hook_name, *args, **kwargs)

  @property
  def user_modes(self):
    modes = {}

    for feature in self.features.values():
      modes.update(feature.USER_MODES)

    return modes

  @property
  def channel_modes(self):
    modes = {}

    for feature in self.features.values():
      modes.update(feature.CHANNEL_MODES)

    return modes

  def get_command(self, name):
    for feature in self.features.values():
      if name in feature.COMMANDS:
        return feature.COMMANDS[name]

    raise KeyError(name)

  def get_feature_locals(self, feature):
    return self.features[feature.NAME].server_locals

  def load_feature(self, name):
    if name[0] == ".":
      name = features.__name__ + name

    try:
      module = imp.reload(importlib.import_module(name))

      try:
        install = module.install
      except AttributeError:
        logger.critical("{} does not name a merc feature!".format(name))
        return

      feature = install(self)
      self.features[feature.NAME] = feature
    except Exception:
      logger.critical("{} could not be loaded.".format(name), exc_info=True)
      return

    logger.info("{} loaded.".format(feature.NAME))

  def unload_feature(self, name):
    if name[0] == ".":
      name = features.__name__ + name

    try:
      feature = self.features[name]
    except KeyError:
      logging.warn("{} could not be loaded as it was not loaded.".format(name))
    else:
      del self.features[name]
      logger.info("{} unloaded.".format(feature.NAME))

  def _unload_all_features(self):
    for feature_name in list(self.features.keys()):
      self.unload_feature(feature_name)

  def _load_configured_features(self):
    for feature_name in self.config["features"]:
      self.load_feature(feature_name)

  def start(self):
    logger.info("""
  Welcome to merc-{}, running for {} on network {}.

  {}\
  """.format(merc.__version__, self.name, self.network_name, self.motd))

    if "ssl" in self.config:
      ssl_ctx = ssl.SSLContext(ssl.PROTOCOL_SSLv23)
      ssl_ctx.load_cert_chain(self.config["ssl"]["cert"],
                              self.config["ssl"]["key"])
    else:
      logger.warn("No SSL configuration found.")
      ssl_ctx = None

    proto_servers = []

    for bind in self.config["bind"]:
      coro = self.loop.create_server(
          lambda: net.Protocol(self), bind["host"], bind["port"],
          ssl=ssl_ctx if bind["ssl"] else None)

      proto_server = self.loop.run_until_complete(coro)
      proto_servers.append(proto_server)

      logger.info("Serving on {}".format(proto_server.sockets[0].getsockname()))

    try:
      self.loop.run_forever()
    except KeyboardInterrupt:
      pass

    proto_server.close()
    self.loop.run_until_complete(asyncio.gather(*[proto_server.wait_closed()
                                               for proto_server in proto_servers]))
    self.loop.close()
