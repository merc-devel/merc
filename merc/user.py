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


class User(object):
  def __init__(self, id, server):
    self.id = id
    self.server = server

    self.nickname = None
    self.username = None
    self.host = None
    self.realname = None

    self.is_negotiating_cap = False
    self.is_registered = False
    self.disconnect_reason = None

    self.channels = {}
    self.modes = {}
    self.feature_locals = {}
    self.capabilities = set()

    self.is_invisible = True
    self.is_irc_operator = False
    self.is_securely_connected = False

    self.creation_time = datetime.datetime.now()
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
  def normalized_nickname(self):
    return util.to_irc_lower(self.nickname) if self.nickname is not None \
                                            else None

  @property
  def is_ready_for_registration(self):
    return self.nickname is not None and self.username is not None and \
           self.host is not None and not self.is_negotiating_cap

  def send(self):
    raise NotImplementedError

  def send_reply(self, message):
    self.send(self.server.name, message)

  def relay_to_user(self, user, message, prefix=None):
    user.send(self.hostmask if prefix is None else prefix, message)

  def relay_to_self(self, message, prefix=None):
    self.send(self.hostmask if prefix is None else prefix, message)

  def relay_to_channel(self, channel, message, prefix=None):
    channel.broadcast(self, self.hostmask if prefix is None else prefix,
                      message)

  def relay_to_all(self, message, prefix=None):
    for channel in self.channels.values():
      self.relay_to_channel(channel, message, prefix)

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

  def get_feature_locals(self, feature):
    return self.feature_locals.setdefault(feature.NAME, {})


class LocalUser(User):
  def __init__(self, server, transport):
    super().__init__(id(self), server)
    self.transport = transport
    self.is_securely_connected = \
        self.transport.get_extra_info("sslcontext") is not None

  def send(self, prefix, msg):
    self.transport.write(msg.emit(self, prefix) + b"\r\n")

  def on_connect(self):
    asyncio.async(self.resolve_hostname_coro(), loop=self.server.loop)

  def on_raw_message(self, prefix, command, params):
    try:
      command_type = self.server.get_command(command)
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
    self.last_activity_time = datetime.datetime.now()
    message.handle_for(self, prefix)
    self.server.run_hooks("after_message", self, message, prefix)

  def on_remove(self):
    for channel_name in list(self.channels):
      self.server.channels.get(channel_name).part(self)
    self.transport.close()

  @asyncio.coroutine
  def resolve_hostname_coro(self):
    host, *_ = self.transport.get_extra_info("peername")
    host, _, _ = host.partition("%")

    self.server.run_hooks("send_server_notice", self,
                          "*** Looking up your hostname...")
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
        self.server.run_hooks("send_server_notice", self,
                              "*** Found your hostname ({})".format(forward))
        self.host = forward
      else:
        self.server.run_hooks("send_server_notice", self,
                              "*** Hostname does not resolve correctly")
    except aiodns.error.DNSError:
      self.server.run_hooks("send_server_notice", self,
                            "*** Couldn't look up your hostname")
      self.host = host

    if self.is_ready_for_registration:
      self.register()

  def register(self):
    if self.server.users.has(self.nickname):
      host, *_ = self.transport.get_extra_info("peername")
      raise errors.Error("Closing Link: {} (Overridden)".format(host))

    self.server.users.add(self)
    self.is_registered = True
    self.server.run_hooks("after_register", self)

  def close(self, reason=None):
    self.disconnect_reason = reason
    self.transport.close()


class RemoteUser(User):
  def __init__(self, id, server, remote_sid):
    super.__init__(id, server)
    self.remote_sid = sid

  def send(self, prefix, msg):
    self.server.run_hooks("send_remote", self, prefix, message)


class UserStore(object):
  def __init__(self, server):
    self.server = server
    self.users = {}

  def local_new(self, transport):
    return LocalUser(self.server, transport)

  def add(self, user):
    self.users[user.normalized_nickname] = user

  def rename(self, user, new_nickname):
    normalized_new_nickname = util.to_irc_lower(new_nickname)

    if normalized_new_nickname in self.users and \
       self.users[normalized_new_nickname] is not user:
      raise errors.NicknameInUse(new_nickname)

    if user.is_registered:
      del self.users[user.normalized_nickname]

    user.nickname = new_nickname

    if user.is_registered:
      self.users[user.normalized_nickname] = user

  def remove(self, user):
    self.server.run_hooks("before_remove_user", user)
    if user.is_registered:
      del self.users[user.normalized_nickname]
    user.on_remove()
    self.server.run_hooks("after_remove_user", user)

  def get(self, name):
    try:
      return self.users[util.to_irc_lower(name)]
    except KeyError:
      raise errors.NoSuchNick(name)

  def has(self, name):
    return util.to_irc_lower(name) in self.users

  def query(self, pattern):
    if "!" not in pattern:
      pattern += "!*@*"

    return (user for user in self.users.values()
                 if user.hostmask_matches(pattern))

  def all(self):
    return self.users.values()

  def count(self):
    return len(self.users)
