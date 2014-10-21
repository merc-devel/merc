import aiodns
import argparse
import asyncio
import datetime
import logging
import operator
import regex
import signal
import ssl

from IPython.extensions import autoreload

import merc

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


class Server(object):
  def __init__(self, config, loop):
    self.loop = loop
    self.config = config

    self.resolver = aiodns.DNSResolver(loop=loop)

    self.creation_time = datetime.datetime.utcnow()

    self.clients = {}
    self.channels = {}

    self.reloader = autoreload.ModuleReloader()
    self.register_signal_handlers()

  @property
  def name(self):
    return self.config["server_name"]

  @property
  def network_name(self):
    return self.config["network_name"]

  @property
  def motd(self):
    return self.config["motd"]

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

  def new_channel(self, name):
    c = channel.Channel(name)
    self.channels[c.normalized_name] = c
    return c

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

    return (client for client in self.clients.values()
                   if client.hostmask_matches(pattern))

def start(config, loop=None):
  if loop is None:
    loop = asyncio.get_event_loop()

  server = Server(config, loop)
  logger.info("""
Welcome to merc-{}, running for {} on network {}.

{}\
""".format(merc.__version__, server.name, server.network_name, server.motd))

  if "ssl" in config:
    ssl_ctx = ssl.SSLContext(ssl.PROTOCOL_SSLv23)
    ssl_ctx.load_cert_chain(config["ssl"]["cert"], config["ssl"]["key"])
  else:
    logger.warn("No SSL configuration found.")
    ssl_ctx = None

  proto_servers = []

  for bind in config["bind"]:
    coro = loop.create_server(
        lambda: net.Protocol(server), bind["host"], bind["port"],
        ssl=ssl_ctx if bind["ssl"] else None)

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
