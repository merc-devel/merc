import asyncio
import collections
import datetime
import fnmatch
import itertools
import regex

from merc import async
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

    host, *_ = protocol.transport.get_extra_info("peername")
    self.host, _, _ = host.partition("%")

    self.protocol = protocol
    self.is_local = True
    self.is_securely_connected = \
        self.protocol.transport.get_extra_info("sslcontext") is not None

    self.registration_latch = async.CountDownLatch(loop=store.app.loop)

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
    if msg.can_send_to(self):
      self.protocol.send(prefix, msg)

  def on_connect(self, app):
    app.run_hooks("user.connect", self)
    asyncio.async(self.registration_latch.wait(), loop=app.loop) \
        .add_done_callback(lambda fut: self.register(app))

  def on_raw_message(self, app, prefix, command_name, params):
    try:
      command_type = app.features.get_user_command(command_name)
    except KeyError:
      if self.is_registered:
        self.send_reply(errors.UnknownCommand(command_name))
    else:
      try:
        self.on_message(app, prefix, command_type.with_params(params))
      except errors.Error as e:
        self.send(None, e)
        self.close("Connection closed")
      except errors.BaseError as e:
        self.send_reply(e)

  def on_message(self, app, prefix, message):
    if message.NAME != "PONG":
      self.last_activity_time = datetime.datetime.now()

    # users are never allowed to send prefixes
    message.handle_for(app, self, None)
    app.run_hooks("user.command", self, message, None)

  def on_disconnect(self, exc):
    self.store.remove(self)

  def on_remove(self, app):
    for channel_name in list(self.channels):
      app.channels.get(channel_name).part(self)

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
    target.send(self.network.local.sid, msg, self)


class UserStore(object):
  def __init__(self, app):
    self.app = app
    self.users = {}
    self.users_by_uid = {}
    self._local_uid_serial = (self.app.network.local.sid + util.uidify(i)
                              for i in itertools.count(0))

  def new_local_user(self, protocol):
    return LocalUser(self, next(self._local_uid_serial), self.app.server.name,
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
    self.remove_unsafe(user)
    user.on_remove(self.app)
    self.app.run_hooks("user.remove", user)

  def remove_unsafe(self, user):
    if user.is_registered:
      del self.users[user.normalized_nickname]
      del self.users_by_uid[user.uid]

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

    for feature in self.app.features.all():
      modes.update(feature.USER_MODES)

    return modes

  @property
  def capabilities(self):
    capabilities = {}

    for feature in self.app.features.all():
      capabilities.update(feature.USER_CAPABILITIES)

    return capabilities
