import asyncio
import logging

from merc import message
from merc import parser



logger = logging.getLogger(__name__)


class Protocol(asyncio.Protocol):
  MAX_BUFFER_SIZE = 2048

  def __init__(self, server):
    self.server = server
    self.buffer = b""

  def connection_made(self, transport):
    self.user = self.server.users.local_new(transport)
    logger.info("Accepted connection from {}".format(
        self.user.transport.get_extra_info("peername")))
    self.user.on_connect(self.server)

  def data_received(self, data):
    self.buffer += data

    if len(self.buffer) > self.MAX_BUFFER_SIZE:
      self.user.close("Excess flood")

    *lines, self.buffer = self.buffer.split(b"\n")

    for line in lines:
      self.handle_line(line.rstrip(b"\r")[:message.Message.MAX_LENGTH])

  def connection_lost(self, exc):
    if exc is not None:
      self.user.disconnect_reason = exc.strerror

    if self.user.disconnect_reason is None:
      self.user.disconnect_reason = "Remote host closed connection"

    self.server.users.remove(self.user)

    logger.info("Lost connection from {}: {}".format(
        self.user.transport.get_extra_info("peername"),
        self.user.disconnect_reason))

  def handle_line(self, line):
    try:
      prefix, command, params = parser.parse_message(line)
    except parser.ParseError:
      return

    self.user.on_raw_message(self.server, prefix, command, params)
