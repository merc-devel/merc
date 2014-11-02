import aiodns
import asyncio
import collections
import datetime
import fnmatch
import ipaddress
import itertools
import regex

from merc import errors
from merc import emitter
from merc import message
from merc import util


class User(object):
  def __init__(self, store, uid, server_name):
    self.uid = uid
    self.store = store

    self.server_name = server_name

    self.nickname = None
    self.username = None
    self.host = None
    self.realname = None

    self.is_negotiating_cap = False

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
  def sid(self):
    return self.uid[:3]

  @property
  def debug_id(self):
    return "user:" + self.uid

  @property
  def link_prefix(self):
    return self.uid

  @property
  def hostmask(self):
    return emitter.emit_hostmask(self.nickname, self.username, self.host)

  def hostmask_matches(self, pattern):
    return regex.match(fnmatch.translate(util.to_irc_lower(pattern)),
                       util.to_irc_lower(self.hostmask)) is not None

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

  def send_reply(self, message, prefix=None):
    self.send(self.server_prefix if prefix is None else prefix, message)

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
  def __init__(self, store, uid, server_name, protocol):
    super().__init__(store, uid, server_name)

    self.is_registered = False

    self.protocol = protocol
    self.is_local = True
    self.is_securely_connected = \
        self.protocol.transport.get_extra_info("sslcontext") is not None

  @property
  def hopcount(self):
    return 0

  @property
  def displayed_nickname(self):
    return self.nickname if self.is_registered else "*"

  @property
  def prefix(self):
    return self.hostmask

  @property
  def server_prefix(self):
    return self.server_name

  def send(self, prefix, msg):
    self.protocol.send(prefix, msg)

  def on_connect(self, app):
    asyncio.async(self.resolve_hostname_coro(app, app.resolver, 5), loop=app.loop)

  def on_raw_message(self, app, prefix, command_name, params):
    try:
      command_type = app.get_user_command(command_name)
    except KeyError:
      if self.is_registered:
        self.send_reply(errors.UnknownCommand(command_name))
    else:
      try:
        self.on_message(app, prefix, command_type.with_params(params))
      except errors.Error as e:
        self.send(None, e)
        self.close(e.reason)
      except errors.BaseError as e:
        self.send_reply(e)

  def on_message(self, app, prefix, message):
    self.last_activity_time = datetime.datetime.now()

    # users are never allowed to send prefixes
    message.handle_for(app, self, None)
    app.run_hooks("user.command", self, message, None)

  def on_disconnect(self, exc):
    self.store.remove(self)

  def on_remove(self, app):
    for channel_name in list(self.channels):
      app.channels.get(channel_name).part(self)

  @asyncio.coroutine
  def resolve_hostname_coro(self, app, resolver, timeout):
    host, *_ = self.protocol.transport.get_extra_info("peername")
    host, _, _ = host.partition("%")

    app.run_hooks("server.notify", self,
                     "*** Looking up your hostname...")
    ip = ipaddress.ip_address(host)

    is_ipv4 = False

    if isinstance(ip, ipaddress.IPv4Address):
      rip = ".".join(reversed(ip.exploded.split("."))) + ".in-addr.arpa."
      is_ipv4 = True
    elif isinstance(ip, ipaddress.IPv6Address):
      rip = ".".join(reversed("".join(ip.exploded.split(":")))) + ".ip6.arpa."

    try:
      forward, *_ = yield from asyncio.wait_for(resolver.query(rip, "PTR"), timeout)
      backward, *_ = yield from asyncio.wait_for(resolver.query(
          forward, "AAAA" if not is_ipv4 else "A"), timeout)

      if ip == ipaddress.ip_address(backward):
        app.run_hooks("server.notify", self,
                         "*** Found your hostname ({})".format(forward))
        self.host = forward
      else:
        app.run_hooks("server.notify", self,
                         "*** Hostname does not resolve correctly")
    except (aiodns.error.DNSError, asyncio.TimeoutError):
      app.run_hooks("server.notify", self,
                       "*** Couldn't look up your hostname")
      self.host = host

    if self.is_ready_for_registration:
      self.register(app)

  def register(self, app):
    if self.store.has(self.nickname):
      host, *_ = self.protocol.transport.get_extra_info("peername")
      raise errors.LinkError("Overridden")

    app.run_hooks("user.register.check", self)
    self.store.add(self)
    self.is_registered = True
    app.run_hooks("user.register", self)

  def close(self, reason=None):
    self.protocol.close(reason)


class RemoteUser(User):
  def __init__(self, store, uid, server_name, hopcount, network):
    super().__init__(store, uid, server_name)

    self.network = network
    self.hopcount = hopcount
    self.is_local = False

  @property
  def is_registered(self):
    return True

  @property
  def prefix(self):
    return self.uid

  @property
  def server_prefix(self):
    return self.sid

  @property
  def displayed_nickname(self):
    return self.uid

  def send(self, prefix, msg):
    target = self.network.get_next_hop(self.network.get(self.server_name))
    target.send(self.network.current.sid, msg, self)


class UserStore(object):
  def __init__(self, app):
    self.app = app
    self.users = {}
    self.users_by_uid = {}
    self._local_uid_serial = (self.app.sid + util.uidify(i)
                              for i in itertools.count(0))

  def new_local_user(self, protocol):
    return LocalUser(self, next(self._local_uid_serial), self.app.server_name,
                     protocol)

  def new_remote_user(self, uid, server_name, hopcount):
    return RemoteUser(self, uid, server_name, hopcount, self.app.network)

  def add(self, user):
    self.users[user.normalized_nickname] = user
    self.users_by_uid[user.uid] = user

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
    self.app.run_hooks("user.remove.check", user)
    if user.is_registered:
      del self.users[user.normalized_nickname]
      del self.users_by_uid[user.uid]
    user.on_remove(self.app)
    self.app.run_hooks("user.remove", user)

  def get(self, name):
    try:
      return self.users[util.to_irc_lower(name)]
    except KeyError:
      raise errors.NoSuchNick(name)

  def get_by_uid(self, uid):
    return self.users_by_uid[uid]

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

  @property
  def modes(self):
    modes = {}

    for feature in self.app.features.values():
      modes.update(feature.USER_MODES)

    return modes

  @property
  def capabilities(self):
    capabilities = {}

    for feature in self.app.features.values():
      capabilities.update(feature.USER_CAPABILITIES)

    return capabilities
