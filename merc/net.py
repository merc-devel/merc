import asyncio
import logging

from merc import message
from merc import parser



logger = logging.getLogger(__name__)


class Protocol(asyncio.Protocol):
  def __init__(self, server):
    self.server = server
    self.buffer = b""

  def connection_made(self, transport):
    self.client = self.server.new_client(transport)
    self.client.on_connect()

  def data_received(self, data):
    self.buffer += data
    *lines, self.buffer = self.buffer.split(b"\n")

    for line in lines:
      self.handle_line(line.rstrip(b"\r")[:message.Message.MAX_LENGTH])

  def connection_lost(self, exc):
    self.server.remove_client(self.client)

  def handle_line(self, line):
    try:
      prefix, command, params = parser.parse_message(line.decode("utf-8"))
    except parser.ParseError:
      return

    self.client.on_raw_message(prefix, command, params)
