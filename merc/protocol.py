import asyncio
import logging

from merc import message
from merc import parser


logger = logging.getLogger(__name__)


class Protocol(asyncio.Protocol):
  MAX_BUFFER_SIZE = 2048

  def __init__(self, app, type):
    self.app = app
    self.type = type
    self.buffer = b""

    self.client = None
    self.disconnect_reason = None

  def local_new(self):
    return {
        "users": self.app.users.new_local_user,
        "servers": self.app.network.new_neighbor
    }[self.type](self)

  def connection_made(self, transport):
    self.transport = transport
    self.client = self.local_new()

    logger.info("Accepted connection from {} (type: {})".format(
        self.transport.get_extra_info("peername"), self.type))
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
    try:
      prefix, command_name, params = parser.parse_message(line)
    except parser.ParseError:
      return

    try:
      command_type = self.app.get_command(command_name)
    except KeyError:
      self.client.on_unknown_command(command_name)
    else:
      self.client.on_message(self.app, prefix, command_type.with_params(params))

  def send(self, prefix, msg):
    self.transport.write(msg.emit(self.client, prefix) + b"\r\n")

  def close(self, reason=None):
    self.disconnect_reason = reason
    self.transport.close()
