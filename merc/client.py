import collections
import re

from merc import emitter
from merc import util
from merc.messages import errors
from merc.messages import message
from merc.messages import registry


class Client(object):
  NICKNAME_REGEX = re.compile(r"^[a-z_\-\[\]\\^{}|`][a-z0-9_\-\[\]\\^{}|`]*$")

  def __init__(self, server, transport):
    self.server = server
    self.transport = transport

    self.nickname = None
    self.username = None
    self.host, *_ = transport.get_extra_info("peername")
    self.realname = None

    self.is_registered = False

    self.channels = set()

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
    if self.NICKNAME_REGEX.match(new_nickname) is None:
      raise errors.ErroneousNickname

    self.server.rename_client(self, new_nickname)

  def register(self):
    self.server.register_client(self)

  def send(self, prefix, message):
    self.transport.write(message.emit(self, prefix).encode("utf-8") + b"\n")

  def send_reply(self, message):
    self.send(self.server.name, message)

  @property
  def prefix(self):
    return ":" + self.hostmask

  def on_raw_message(self, prefix, command, params):
    try:
      self.on_message(prefix, registry.REGISTRY[command].with_params(params))
    except errors.Error as e:
      self.send(None, e)
      self.transport.close()
    except errors.ErrorMessage as e:
      self.send_reply(e)

  def on_message(self, prefix, message):
    message.handle_for(self, prefix)

  def join(self, channel_name):
    self.server.join_channel(self, channel_name)

  __hash__ = id
