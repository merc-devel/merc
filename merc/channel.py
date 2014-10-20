import re

from merc.messages import errors


class Channel(object):
  CHANNEL_REGEX = re.compile("^#[^, ]*$")

  def __init__(self, name):
    if self.CHANNEL_REGEX.match(name) is None:
      raise errors.NoSuchChannel(name)

    self.name = name

    self.clients = set()

  def broadcast(self, prefix, message):
    for client in self.clients:
      client.send(prefix, message)

  def join(self, client, key=None):
    self.clients.add(client)
    client.channels.add(self)

  def part(self, client):
    self.clients.remove(client)
    client.channels.remove(self)

  __hash__ = lambda self: id(self)
