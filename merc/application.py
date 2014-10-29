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
import sys
import yaml

import passlib.context

import merc

from merc import config
from merc import config_format
from merc import channel
from merc import errors
from merc import features
from merc import message
from merc import protocol
from merc import server
from merc import user
from merc import util

logger = logging.getLogger(__name__)


class Application(object):
  def __init__(self, config_filename, loop=None):
    if loop is None:
      loop = asyncio.get_event_loop()

    self.loop = loop

    self.features = {}
    self.bindings = []

    self.config_filename = config_filename
    self.reload_config()

    self.resolver = aiodns.DNSResolver(loop=loop)
    self.crypt_context = passlib.context.CryptContext(
        schemes=self.config["crypto"]["hash_schemes"])

    self.creation_time = datetime.datetime.now()

    self.users = user.UserStore(self)
    self.channels = channel.ChannelStore(self)
    self.network = server.Network(self)

    self.register_signal_handlers()

  def create_tls_context(self):
    if self.config["tls"]:
      tls_ctx = ssl.SSLContext(ssl.PROTOCOL_SSLv23)
      tls_ctx.load_cert_chain(self.config["tls"]["cert"],
                              self.config["tls"]["key"])

      # We disable SSLv2, SSLv3, and compression (CRIME).
      tls_ctx.options |= ssl.OP_NO_SSLv2 | ssl.OP_NO_SSLv3 | \
                         getattr(ssl, "OP_NO_COMPRESSION", 0)
      return tls_ctx
    else:
      logger.warn("No TLS configuration found.")
      return None

  def check_config(self, cfg):
    config.validate(cfg, config_format.Config)

  def reload_config(self):
    self._unload_all_features()
    with open(self.config_filename, "r") as f:
      config = yaml.safe_load(f)
    try:
      self.check_config(config)
    except:
      raise
    else:
      self.config = config
    finally:
      self._load_configured_features()

  def rehash(self):
    @asyncio.coroutine
    def coro():
      self.reload_config()
      yield from self.unbind()
      yield from self.bind()
    return asyncio.async(coro(), loop=self.loop)

  @property
  def server_name(self):
    return self.config["server"]["name"]

  @property
  def sid(self):
    return self.config["server"]["sid"]

  @property
  def version(self):
    return util.get_version()

  @property
  def network_name(self):
    return self.config["server"]["network_name"]

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

  def run_hooks(self, hook_name, *args, **kwargs):
    for feature in self.features.values():
      feature.run_hooks(hook_name, self, *args, **kwargs)

  def get_user_command(self, name):
    for feature in self.features.values():
      if name in feature.USER_COMMANDS:
        return feature.USER_COMMANDS[name]

    raise KeyError(name)

  def get_server_command(self, name):
    for feature in self.features.values():
      if name in feature.SERVER_COMMANDS:
        return feature.SERVER_COMMANDS[name]

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

      install(self)
    except Exception:
      logger.critical("{} could not be loaded.".format(name), exc_info=True)
      return

  def install_feature(self, feature):
    self.features[feature.NAME] = feature
    logger.info("{} installed.".format(feature.NAME))

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

  @asyncio.coroutine
  def unbind(self):
    wait_for = []

    while self.bindings:
      binding = self.bindings.pop()
      logger.info("Unbinding from {}".format(binding.sockets[0].getsockname()))
      binding.close()
      wait_for.append(binding.wait_closed())

    yield from asyncio.gather(*wait_for)

  @asyncio.coroutine
  def bind(self):
    if any(bind["tls"] for bind in self.config["bind"]):
      tls_ctx = self.create_tls_context()

    for bind in self.config["bind"]:
      protocol_factory = {
          "users": protocol.UserProtocol,
          "servers": protocol.LinkProtocol
      }[bind["type"]]

      binding = yield from self.loop.create_server(
          lambda protocol_factory=protocol_factory: protocol_factory(self),
          bind["host"], bind["port"],
          ssl=tls_ctx if bind["tls"] else None)
      logger.info("Binding to {}: {}".format(binding.sockets[0].getsockname(),
                                             protocol_factory.__name__))

      self.bindings.append(binding)

  def start(self):
    logger.info("Welcome to merc-{}, running for {} on network {}.".format(
        merc.__version__, self.server_name, self.network_name))

    self.loop.run_until_complete(self.bind())

    try:
      self.loop.run_forever()
    except KeyboardInterrupt:
      pass

    self.loop.run_until_complete(self.unbind())
    self.loop.close()


def main():
  import argparse
  import coloredlogs
  import logging
  import yaml

  coloredlogs.install(level=logging.INFO)
  logging.getLogger("asyncio").setLevel(logging.WARN)

  parser = argparse.ArgumentParser(
      formatter_class=argparse.ArgumentDefaultsHelpFormatter)
  parser.add_argument("--config", "-c", help="file to load configuration from",
                      default="merc.conf")

  args = parser.parse_args()

  Application(args.config).start()
