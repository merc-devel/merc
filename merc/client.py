import collections
import re

from merc import emitter
from merc import util
from merc.messages import errors
from merc.messages import message
from merc.messages import registry


class Client(object):
  NICKNAME_REGEX = re.compile(r"^[a-z_\-\[\]\\^{}|`][a-z0-9_\-\[\]\\^{}|`]*$",
                              re.I)
  MAX_NICKNAME_LENGTH = 16
  MODES = set("i")

  def __init__(self, server, transport):
    self.id = id(self)

    self.server = server
    self.transport = transport

    self.nickname = None
    self.username = None
    self.host, *_ = transport.get_extra_info("peername")
    self.realname = None

    self.is_registered = False

    self.channels = {}

  @property
  def hostmask(self):
    return emitter.emit_hostmask(self.nickname, self.username, self.host)

  @property
  def displayed_nickname(self):
    return self.nickname if self.is_registered else "*"

  @property
  def normalized_nickname(self):
    return util.to_irc_lower(self.nickname) if self.nickname is not None \
                                            else None

  def rename(self, new_nickname):
    if self.NICKNAME_REGEX.match(new_nickname) is None or \
        len(new_nickname) > self.MAX_NICKNAME_LENGTH:
      raise errors.ErroneousNickname

    self.server.rename_client(self, new_nickname)

  def register(self):
    self.server.register_client(self)

  def send(self, prefix, message):
    self.transport.write(message.emit(self, prefix).encode("utf-8") + b"\n")

  def send_reply(self, message):
    self.send(self.server.name, message)

  def relay_to_client(self, client, message, prefix=None):
    client.send(self.hostmask if prefix is None else prefix, message)

  def relay_to_self(self, message, prefix=None):
    self.send(self.hostmask if prefix is None else prefix, message)

  def relay_to_channel(self, channel, message, prefix=None):
    channel.broadcast(self, self.hostmask if prefix is None else prefix,
                      message)

  def relay_to_all(self, message, prefix=None):
    for channel in self.channels.values():
      self.relay_to_channel(channel, message, prefix)

  def on_raw_message(self, prefix, command, params):
    try:
      command_type = registry.REGISTRY[command]
    except KeyError:
      if self.is_registered:
        self.send_reply(errors.UnknownCommand(command))
    else:
      try:
        self.on_message(prefix, command_type.with_params(params))
      except errors.Error as e:
        self.send(None, e)
        self.transport.close()
      except errors.ErrorMessage as e:
        self.send_reply(e)

  def on_message(self, prefix, message):
    message.handle_for(self, prefix)

  def on_close(self):
    for channel_name in list(self.channels):
      self.server.part_channel(self, channel_name)

  def join(self, channel_name):
    self.server.join_channel(self, channel_name)
