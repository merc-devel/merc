import aiodns
import asyncio
import collections
import datetime
import fnmatch
import ipaddress
import regex

from merc import errors
from merc import emitter
from merc import message
from merc import util
from merc.features import mode
from merc.features import ping
from merc.features import privmsg
from merc.features import welcome


class Client(object):
  NICKNAME_REGEX = regex.compile(r"^[\p{L}\p{So}_\[\]\\^{}|`][\p{L}\p{So}\p{N}_\[\]\\^{}|`-]*$")
  MAX_NICKNAME_LENGTH = 16

  PING_TIMEOUT = datetime.timedelta(seconds=240)
  PONG_TIMEOUT = datetime.timedelta(seconds=20)

  def __init__(self, server, transport):
    self.id = id(self)

    self.server = server
    self.transport = transport

    self.nickname = None
    self.username = None
    self.host = None
    self.realname = None

    self.away_message = None

    self.is_registered = False
    self.disconnect_reason = None

    self.channels = {}

    self.is_invisible = True
    self.is_irc_operator = False

    self.creation_time = datetime.datetime.utcnow()
    self.last_activity_time = self.creation_time

    self.ping_check_handle = None
    self.pong_check_handle = None

  @property
  def hostmask(self):
    return emitter.emit_hostmask(self.nickname, self.username, self.host)

  def hostmask_matches(self, pattern):
    return regex.match(fnmatch.translate(util.to_irc_lower(pattern)),
                       util.to_irc_lower(self.hostmask)) is not None

  @property
  def displayed_nickname(self):
    return self.nickname if self.is_registered else "*"

  @property
  def is_away(self):
    return self.away_message is not None

  @property
  def is_securely_connected(self):
    return self.transport.get_extra_info("sslcontext") is not None

  @property
  def normalized_nickname(self):
    return util.to_irc_lower(self.nickname) if self.nickname is not None \
                                            else None

  def rename(self, new_nickname):
    if self.NICKNAME_REGEX.match(new_nickname) is None or \
        len(new_nickname) > self.MAX_NICKNAME_LENGTH:
      raise errors.ErroneousNickname

    self.server.rename_client(self, new_nickname)

  @property
  def is_ready_for_registration(self):
    return self.nickname is not None and self.username is not None and \
           self.host is not None

  def set_mode(self, client, mode, param=None):
    try:
      set, _ = self.MODES[mode]
    except KeyError:
      raise errors.UmodeUnknownFlag

    return set(self, param)

  def unset_mode(self, client, mode, param=None):
    try:
      _, unset = self.MODES[mode]
    except KeyError:
      raise errors.UmodeUnknownFlag

    return unset(self, param)

  def register(self):
    self.server.register_client(self)
    self.reschedule_ping_check()

  def send(self, prefix, msg):
    raw = msg.emit(self, prefix).encode(
        "utf-8")[:message.Message.MAX_LENGTH].decode("utf-8", "ignore")
    self.transport.write(raw.encode("utf-8") + b"\r\n")

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

  @asyncio.coroutine
  def resolve_hostname_coro(self):
    host, *_ = self.transport.get_extra_info("peername")

    self.send_reply(privmsg.Notice("*", "*** Looking up your hostname..."))
    ip = ipaddress.ip_address(host)

    is_ipv4 = False

    if isinstance(ip, ipaddress.IPv4Address):
      rip = ".".join(reversed(ip.exploded.split("."))) + ".in-addr.arpa."
      is_ipv4 = True
    elif isinstance(ip, ipaddress.IPv6Address):
      rip = ".".join(reversed("".join(ip.exploded.split(":")))) + ".ip6.arpa."

    try:
      forward, *_ = yield from self.server.resolver.query(rip, "PTR")
      backward, *_ = yield from self.server.resolver.query(
          forward, "AAAA" if not is_ipv4 else "A")

      if ip == ipaddress.ip_address(backward):
        self.send_reply(privmsg.Notice(
            "*", "*** Found your hostname ({})".format(forward)))
        self.host = forward
      else:
        self.send_reply(privmsg.Notice(
            "*", "*** Hostname does not resolve correctly"))
    except aiodns.error.DNSError:
      self.send_reply(privmsg.Notice(
          "*", "*** Couldn't look up your hostname"))
      self.host = host

    if self.is_ready_for_registration:
      self.register()

  def reschedule_ping_check(self):
    if self.ping_check_handle is not None:
      self.ping_check_handle.cancel()

    if self.pong_check_handle is not None:
      self.pong_check_handle.cancel()

    def ping_check():
      self.send(None, ping.Ping(self.server.name))
      self.pong_check_handle = self.server.loop.call_later(
          self.PONG_TIMEOUT.total_seconds(), pong_check)

    def pong_check():
      self.close("Ping timeout: {} seconds".format(
          int(self.PING_TIMEOUT.total_seconds())))

    self.ping_check_handle = self.server.loop.call_later(
        self.PING_TIMEOUT.total_seconds(), ping_check)

  def on_connect(self):
    asyncio.async(self.resolve_hostname_coro(), loop=self.server.loop)

  def on_raw_message(self, prefix, command, params):
    try:
      command_type = message.Command.REGISTRY[command]
    except KeyError:
      if self.is_registered:
        self.send_reply(errors.UnknownCommand(command))
    else:
      try:
        self.on_message(prefix, command_type.with_params(params))
      except errors.Error as e:
        self.send(None, e)
        self.transport.close()
      except errors.BaseError as e:
        self.send_reply(e)

  def on_message(self, prefix, message):
    self.last_activity_time = datetime.datetime.utcnow()

    if self.is_registered:
      self.reschedule_ping_check()

    message.handle_for(self, prefix)

  def on_close(self):
    self.relay_to_all(welcome.Quit(self.disconnect_reason))

    for channel_name in list(self.channels):
      self.server.part_channel(self, channel_name)

  def close(self, reason=None):
    self.disconnect_reason = reason
    self.transport.close()

  def is_in_channel(self, channel):
    return channel.normalized_name in self.channels

  def can_see_channel(self, channel):
    return not channel.is_secret or self.is_in_channel(channel)

  def get_channels_visible_for(self, other):
    return (channel for channel in other.channels.values()
                    if self.can_see_channel(channel))

  def check_is_irc_operator(self):
    if not self.is_irc_operator:
      raise errors.NoPrivileges

  def mutate_invisible(self, client, flag):
    if self.is_invisible == flag:
      return False

    self.is_invisible = flag
    return True

  def mutate_irc_operator(self, client, flag):
    if not self.is_irc_operator:
      return False

    self.is_irc_operator = flag
    return True

  @property
  def modes(self):
    modes = {}

    if self.is_invisible:
      modes["i"] = True

    if self.is_securely_connected:
      modes["Z"] = True

    if self.is_irc_operator:
      modes["o"] = True

    return modes

  MODES = {
    "i": util.make_flag_pair(mutate_invisible),
    "o": util.make_flag_pair(mutate_irc_operator),
    "Z": util.make_immutable_flag_pair()
  }
