import argparse
import asyncio
import logging

from merc import channel
from merc import client
from merc import net
from merc import util
from merc.messages import errors

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
  return parser


class Server(object):
  def __init__(self, name, network_name):
    self.name = name
    self.network_name = network_name

    self.clients = {}
    self.channels = {}

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

    # then some registration stuff

  def rename_client(self, client, new_nickname):
    normalized_new_nickname = util.to_irc_lower(new_nickname)

    if normalized_new_nickname in self.clients and \
       self.clients[normalized_new_nickname] is not client:
      raise errors.NicknameInUse(new_nickname)

    if client.is_registered:
      del self.clients[client.normalized_nickname]
      self.clients[client.normalized_nickname] = client

    client.nickname = new_nickname

  def remove_client(self, client):
    if client.is_registered:
      del self.clients[client.normalized_nickname]

  def get_or_new_channel(self, name):
    normalized_channel_name = util.to_irc_lower(name)

    if normalized_channel_name not in self.channels:
      self.channels[normalized_channel_name] = channel.Channel(name)

    return self.channels[normalized_channel_name]

  def join_channel(self, client, name):
    channel = self.get_or_new_channel(name)
    channel.clients.add(client)
    return channel

  def part_channel(self, client, name):
    channel = self.get_or_new_channel(name)
    channel.clients.remove(client)

    if not channel.clients:
      del self.channels[util.to_irc_lower(name)]

    return channel


def start(config, loop=None):
  if loop is None:
    loop = asyncio.get_event_loop()

  server = Server(config.server_name, config.network_name)
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
