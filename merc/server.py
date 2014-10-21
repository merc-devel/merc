import aiodns
import argparse
import asyncio
import datetime
import fnmatch
import logging
import operator
import regex
import signal
import ssl

from IPython.extensions import autoreload

from merc import channel
from merc import errors
from merc import client
from merc import message
from merc import net
from merc import util
from merc.features import welcome

# This initializes all the commands.
from merc.features import *

logger = logging.getLogger(__name__)


def make_config_parser():
  parser = argparse.ArgumentParser()
  parser.add_argument("--server-name", help="The name of the server.",
                      default="irc.example.org")
  parser.add_argument("--network-name", help="The name of the network.",
                      default="ExampleNet")
  parser.add_argument("--ssl-cert", help="The SSL certificate to use.",
                      default="server.crt")
  parser.add_argument("--ssl-key", help="The SSL certificate key to use.",
                      default="server.key")
  parser.add_argument("--bind", help="Hosts to bind to.",
                      default="127.0.0.1:6667,127.0.0.1:+6697," +
                              "::1:6667,::1:+6697")
  parser.add_argument("--motd-file", help="MOTD file to read the MOTD from.",
                      default="motd.txt")
  return parser


class Server(object):
  def __init__(self, name, network_name, motd, loop):
    self.loop = loop

    self.name = name
    self.network_name = network_name
    self.motd = motd

    self.resolver = aiodns.DNSResolver(loop=loop)

    self.creation_time = datetime.datetime.utcnow()

    self.clients = {}
    self.channels = {}

    self.reloader = autoreload.ModuleReloader()
    self.register_signal_handlers()

  def register_signal_handlers(self):
    signal.signal(signal.SIGUSR1, lambda signum, frame: self.reload_code())

  def reload_code(self):
    # very questionable
    logger.warn("Reloading code (you should probably restart instead)...")
    self.reloader.check(True)

  @property
  def isupport(self):
    return {
      "CHANTYPES": "#",
      "NETWORK": self.network_name,
      "CASEMAPPING": "unicode",
      "PREFIX": "({}){}".format(channel.ChannelUser.ROLE_MODES,
                                channel.ChannelUser.ROLE_CHARS),
      "CHARSET": "utf-8",
      "NICKLEN": client.Client.MAX_NICKNAME_LENGTH,
      "TOPICLEN": channel.Channel.MAX_TOPIC_LENGTH
    }

  def new_client(self, transport):
    c = client.Client(self, transport)

    logger.info("Accepted connection from {}".format(
        transport.get_extra_info("peername")))

    return c

  def register_client(self, client):
    if client.normalized_nickname in self.clients:
      host, *_ = client.transport.get_extra_info("peername")
      raise errors.Error("Closing Link: {} (Overridden)".format(host))

    self.clients[client.normalized_nickname] = client
    client.is_registered = True

    welcome.welcome(client, self)

  def rename_client(self, client, new_nickname):
    normalized_new_nickname = util.to_irc_lower(new_nickname)

    if normalized_new_nickname in self.clients and \
       self.clients[normalized_new_nickname] is not client:
      raise errors.NicknameInUse(new_nickname)

    if client.is_registered:
      del self.clients[client.normalized_nickname]

    client.nickname = new_nickname

    if client.is_registered:
      self.clients[client.normalized_nickname] = client

  def remove_client(self, client):
    if client.is_registered:
      del self.clients[client.normalized_nickname]
    client.on_close()

  def get_client(self, name):
    try:
      return self.clients[util.to_irc_lower(name)]
    except KeyError:
      raise errors.NoSuchNick(name)

  def get_channel(self, name):
    try:
      return self.channels[util.to_irc_lower(name)]
    except KeyError:
      raise errors.NoSuchNick(name)

  def get_or_new_channel(self, name):
    normalized_channel_name = util.to_irc_lower(name)

    if normalized_channel_name not in self.channels:
      self.channels[normalized_channel_name] = channel.Channel(name)

    return self.get_channel(name)

  def remove_channel(self, channel):
    del self.channels[channel.normalized_name]

  def part_channel(self, client, name):
    channel = self.get_channel(name)
    channel.part(client)

    if not channel.users:
      self.remove_channel(channel)

    return channel

  def query_clients(self, pattern):
    if "!" not in pattern:
      pattern += "!*@*"

    expr = regex.compile(fnmatch.translate(util.to_irc_lower(pattern)))

    for client in self.clients.values():
      if expr.match(util.to_irc_lower(client.hostmask)) is not None:
        yield client

def start(config, loop=None):
  if loop is None:
    loop = asyncio.get_event_loop()

  with open(config.motd_file, "r") as f:
    motd = f.read()

  server = Server(config.server_name, config.network_name, motd, loop)

  ssl_ctx = ssl.SSLContext(ssl.PROTOCOL_SSLv23)
  ssl_ctx.load_cert_chain(config.ssl_cert, config.ssl_key)

  proto_servers = []

  for bind_spec in config.bind.split(","):
    host, _, port = bind_spec.rpartition(":")
    cur_ssl_ctx = None

    if port[0] == "+":
      port = port[1:]
      cur_ssl_ctx = ssl_ctx

    coro = loop.create_server(
        lambda: net.Protocol(server), host, port, ssl=cur_ssl_ctx)

    proto_server = loop.run_until_complete(coro)
    proto_servers.append(proto_server)

    logger.info("Serving on {}".format(proto_server.sockets[0].getsockname()))

  try:
    loop.run_forever()
  except KeyboardInterrupt:
    pass

  proto_server.close()
  loop.run_until_complete(asyncio.gather(*[proto_server.wait_closed()
                                           for proto_server in proto_servers]))
  loop.close()
