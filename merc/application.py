import aiodns
import asyncio
import datetime
import logging
import signal
import yaml

import passlib.context


from merc import config
from merc import config_format
from merc import channel
from merc import feature
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

    self.creation_time = datetime.datetime.now()

    self.resolver = aiodns.DNSResolver(loop=loop)
    self.features = feature.FeatureLoader(self)
    self.users = user.UserStore(self)
    self.channels = channel.ChannelStore(self)
    self.network = server.Network(self)
    self.server = None
    self.crypt_context = None

    self.config = None
    self.config_filename = config_filename
    self.reload_config()

    self.register_signal_handlers()


  def check_config(self, cfg):
    config.validate(cfg, config_format.Config)

  def reload_config(self):
    self.features.unload_all()
    with open(self.config_filename, "r") as f:
      config = yaml.safe_load(f)

    try:
      self.check_config(config)
      for feature_name in config["features"]:
        self.features.load(feature_name)
      self.run_hooks("config.check", config)
    except:
      logger.critical("Configuration invalid.")
      self.features.unload_all()

      if self.config:
        logger.critical("Reloading old configuration.")
        for feature_name in self.config["features"]:
          self.features.load(feature_name)
      raise
    else:
      self.config = config
    finally:
      if self.config:
        self.update_from_config()

  def update_from_config(self):
    self.network.update_local(
        self.loop,
        self.config["server"]["name"],
        self.config["server"]["description"],
        self.config["server"]["sid"])
    self.server = self.network.local

    self.crypt_context = passlib.context.CryptContext(
        schemes=self.config["crypto"]["hash_schemes"])

  def rehash(self):
    @asyncio.coroutine
    def coro():
      yield from self.unbind()
      self.reload_config()
      yield from self.bind()
    return asyncio.async(coro(), loop=self.loop)

  @asyncio.coroutine
  def bind(self):
    yield from self.network.local.bind(self, self.config["bind"])

  @asyncio.coroutine
  def unbind(self):
    yield from self.network.local.unbind()


  @property
  def version(self):
    return util.get_version()

  @property
  def network_name(self):
    return self.config["server"]["network_name"]

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
    for feature in self.features.all():
      feature.run_hooks(hook_name, self, *args, **kwargs)

  def get_feature_locals(self, feature):
    return self.features[feature.NAME].server_locals


  def start(self):
    logger.info("Welcome to merc-{}, running for {} ({}) on network {}.".format(
        util.get_version(), self.config["server"]["name"],
        self.config["server"]["sid"], self.config["server"]["network_name"]))

    self.loop.run_until_complete(self.bind())
    self._autoconnect_links()

    try:
      self.loop.run_forever()
    except KeyboardInterrupt:
      pass

    self.loop.run_until_complete(self.unbind())
    self.loop.close()

  def _autoconnect_links(self):
    for server_name, link_spec in self.config["links"].items():
      if link_spec["autoconnect"]:
        self.network.connect(server_name)


def main():
  import argparse
  import coloredlogs

  parser = argparse.ArgumentParser(
      formatter_class=argparse.ArgumentDefaultsHelpFormatter)
  parser.add_argument("--config", "-c", help="file to load configuration from",
                      default="merc.conf")
  parser.add_argument("--verbose", "-v", help="enable verbose (debug) logging",
                      action="store_true", default=False)

  args = parser.parse_args()

  coloredlogs.install(level=logging.DEBUG if args.verbose else logging.INFO)
  logging.getLogger("asyncio").setLevel(logging.WARN)

  try:
    app = Application(args.config)
    app.start()
  except config.ParseError as e:
    logging.fatal('Could not load configuration file, aborting.')
    logging.fatal(e)
  except Exception as e:
    logging.fatal('Could not initialize merc, aborting.')
    logging.fatal(e)
