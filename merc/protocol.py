import asyncio
import logging

from merc import message
from merc import parser


logger = logging.getLogger(__name__)


class Protocol(asyncio.Protocol):
  MAX_BUFFER_SIZE = 2048

  def __init__(self, app):
    self.app = app
    self.buffer = b""

    self.client = None
    self.disconnect_reason = None

  def connection_made(self, transport):
    self.transport = transport
    self.client = self.local_new()

    logger.info("Connection made on {} (type: {})".format(
        self.transport.get_extra_info("peername"), self.__class__.__name__))
    self.client.on_connect(self.app)

  def data_received(self, data):
    self.buffer += data

    if len(self.buffer) > self.MAX_BUFFER_SIZE:
      self.close("Excess flood")

    *lines, self.buffer = self.buffer.split(b"\n")

    for line in lines:
      self.handle_line(line.rstrip(b"\r")[:message.Message.MAX_LENGTH])

  def connection_lost(self, exc):
    if exc is not None:
      self.disconnect_reason = exc.strerror

    if self.disconnect_reason is None:
      self.disconnect_reason = "Remote host closed connection"

    self.client.on_disconnect(exc)

    logger.info("Lost connection from {}: {}".format(
        self.transport.get_extra_info("peername"),
        self.disconnect_reason))

  def handle_line(self, line):
    logger.debug("[{}] <- {}".format(self.client.debug_id, line))
    try:
      prefix, command_name, params = parser.parse_message(line)
    except parser.ParseError:
      return

    self.client.on_raw_message(self.app, prefix, command_name, params)

  def send(self, prefix, msg, source=None):
    if source is None:
      source = self.client

    raw = msg.emit(source, prefix)
    logger.debug("[{}] -> {}".format(self.client.debug_id, raw))
    self.transport.write(raw + b"\r\n")

  def close(self, reason=None):
    self.disconnect_reason = reason
    self.transport.close()


class UserProtocol(Protocol):
  def local_new(self):
    return self.app.users.new_local_user(self)


class LinkProtocol(Protocol):
  def local_new(self):
    return self.app.network.new_neighbor(self)
