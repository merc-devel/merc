import aiodns
import argparse
import asyncio
import datetime
import logging
import operator

from merc import channel
from merc import client
from merc import net
from merc import util
from merc.messages import commands
from merc.messages import errors
from merc.messages import replies

logger = logging.getLogger(__name__)


def make_config_parser():
  parser = argparse.ArgumentParser()
  parser.add_argument("--server-name", help="The name of the server.",
                      default="irc.example.org")
  parser.add_argument("--network-name", help="The name of the network.",
                      default="ExampleNet")
  parser.add_argument("--bind-host", help="Host to bind to.",
                      default="localhost")
  parser.add_argument("--bind-port", help="Port to bind to.",
                      default=6667)
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

  @property
  def isupport(self):
    user_channel_modes = []
    user_channel_chars = []

    for i, m in sorted(channel.ChannelUser.ROLE_MODES.items(),
                       key=operator.itemgetter(0), reverse=True):
      user_channel_modes.append(m)
      user_channel_chars.append(channel.ChannelUser.ROLE_CHARS[i])

    return {
      "CHANTYPES": "#",
      "NETWORK": self.network_name,
      "CASEMAPPING": "unicode",
      "PREFIX": "({}){}".format("".join(user_channel_modes),
                                "".join(user_channel_chars)),
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

    client.send_reply(replies.Welcome())
    client.send_reply(replies.YourHost())
    client.send_reply(replies.Created())
    client.send_reply(replies.MyInfo())
    client.send_reply(replies.ISupport(self.isupport))
    client.on_message(client.hostmask, commands.LUsers())
    client.on_message(client.hostmask, commands.Motd())

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
    return self.clients[util.to_irc_lower(name)]

  def get_channel(self, name):
    return self.channels[util.to_irc_lower(name)]

  def get_or_new_channel(self, name):
    normalized_channel_name = util.to_irc_lower(name)

    if normalized_channel_name not in self.channels:
      self.channels[normalized_channel_name] = channel.Channel(name)

    return self.channels[normalized_channel_name]

  def join_channel(self, client, name, key):
    channel = self.get_or_new_channel(name)
    channel.join(client, key)
    return channel

  def part_channel(self, client, name):
    channel = self.get_channel(name)
    channel.part(client)

    if not channel.users:
      del self.channels[util.to_irc_lower(name)]

    return channel


def start(config, loop=None):
  if loop is None:
    loop = asyncio.get_event_loop()

  with open(config.motd_file, "r") as f:
    motd = f.read()

  server = Server(config.server_name, config.network_name, motd, loop)
  coro = loop.create_server(
      lambda: net.Protocol(server), config.bind_host, config.bind_port)
  proto_server = loop.run_until_complete(coro)

  logger.info("Serving on {}".format(proto_server.sockets[0].getsockname()))
  try:
    loop.run_forever()
  except KeyboardInterrupt:
    pass

  proto_server.close()
  loop.run_until_complete(proto_server.wait_closed())
  loop.close()
