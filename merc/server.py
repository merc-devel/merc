import asyncio
import logging
import ssl
import networkx
import networkx.algorithms

from merc import errors
from merc import protocol


logger = logging.getLogger(__name__)


class Server(object):
  def __init__(self, network):
    self.network = network
    self.feature_locals = {}

  @property
  def debug_id(self):
    return "server:" + (self.sid if self.sid is not None else "?")

  def get_feature_locals(self, feature):
    return self.feature_locals.setdefault(feature.NAME, {})


class LocalServer(Server):
  def __init__(self, network, loop, name, description, sid):
    super().__init__(network)

    self.loop = loop
    self.name = name
    self.description = description
    self.sid = sid
    self.hopcount = 0
    self.bindings = []
    self.was_proposed = True


  def create_tls_context(self, cert, key):
    tls_ctx = ssl.SSLContext(ssl.PROTOCOL_SSLv23)
    tls_ctx.load_cert_chain(cert, key)

    # We disable SSLv2, SSLv3, and compression (CRIME).
    tls_ctx.options |= ssl.OP_NO_SSLv2 | ssl.OP_NO_SSLv3 | \
                         getattr(ssl, "OP_NO_COMPRESSION", 0)
    return tls_ctx


  @asyncio.coroutine
  def bind(self, app, binds):
    for bind in binds:
      if bind["tls"]:
        tls_ctx = self.create_tls_context(**bind["tls"])
      else:
        tls_ctx = None

      protocol_factory = {
          "users": protocol.UserProtocol,
          "servers": protocol.LinkProtocol
      }[bind["type"]]

      binding = yield from self.loop.create_server(
          lambda protocol_factory=protocol_factory: protocol_factory(app),
          bind["host"], bind["port"], ssl=tls_ctx)
      logger.info("Binding to {}: {}".format(binding.sockets[0].getsockname(),
                                             protocol_factory.__name__))

      self.bindings.append(binding)

  @asyncio.coroutine
  def unbind(self):
    wait_for = []

    while self.bindings:
      binding = self.bindings.pop()
      logger.info("Unbinding from {}".format(binding.sockets[0].getsockname()))
      binding.close()
      wait_for.append(binding.wait_closed())

    yield from asyncio.gather(*wait_for)


class Neighbor(Server):
  def __init__(self, network, protocol):
    super().__init__(network)
    self.protocol = protocol

    self.name = None
    self.password = None
    self.description = None
    self.sid = None
    self.hopcount = None

    self.is_registered = False
    self.was_proposed = False

  def register(self, app):
    app.run_hooks("server.register.check", self)
    try:
      self.network.add_neighbor(self)
    except KeyError:
      raise errors.LinkError("Name collision")
    app.run_hooks("server.register", self)
    self.is_registered = True

  @property
  def displayed_nickname(self):
    return "*"

  def send(self, prefix, msg):
    self.protocol.send(prefix, msg)

  def on_connect(self, app):
    pass

  def on_raw_message(self, app, prefix, command_name, params):
    try:
      command_type = app.features.get_server_command(command_name)
    except KeyError:
      if command_name.isnumeric():
        # TODO: handle numerics
        return
      self.send(None, errors.LinkError("Unknown command"))
      self.protocol.close("Unknown command")
    else:
      try:
        self.on_message(app, prefix, command_type.with_params(params))
      except errors.Error as e:
        self.send(None, e)
        self.protocol.close(e.reason)
      except errors.BaseError as e:
        self.send(None, e)

  def on_message(self, app, prefix, message):
    message.handle_for(app, self, prefix)

  def on_disconnect(self, exc):
    if self.is_registered:
      self.network.remove(self)


class NonNeighbor(Server):
  # alternative name: ThatOneOddFamilyMemberYouNeverBotheredWithBecauseTheySeemWeirdButNotWeirdInTheGoodWay
  def __init__(self, network, name, hopcount, sid, description):
    super().__init__(network)
    self.name = name
    self.hopcount = hopcount
    self.sid = sid
    self.description = description

  def send(self, prefix, msg):
    target = self.network.get_next_hop(self.network.get(self.name))
    target.send(prefix, msg)


class Network(object):
  def __init__(self, app):
    self.app = app

    self.tree = networkx.Graph()
    self.servers_by_sid = {}

    self.local = None

  def update_local(self, loop, name, desc, sid):
    self.local = LocalServer(self, loop, name, desc, sid)
    self.upsert(self.local)

  @property
  def name(self):
    return self.app.network_name

  @property
  def sids(self):
    return self.servers_by_sid.keys()

  def get_send_password(self, target):
    return self.app.config["links"][target.name]["send_password"]

  def new_neighbor(self, protocol):
    return Neighbor(self, protocol)

  def new_non_neighbor(self, name, hopcount, sid, description):
    return NonNeighbor(self, name, hopcount, sid, description)

  def add(self, server):
    if server.name in self.tree.node:
      raise KeyError(server.name)
    self.upsert(server)

  def upsert(self, server):
    self.tree.add_node(server.name, {"server": server})
    self.servers_by_sid[server.sid] = server

  def has(self, name):
    return name in self.tree

  def has_by_sid(self, sid):
    return sid in self.servers_by_sid

  def connect(self, server_name):
    link_spec = self.app.config["links"][server_name]

    @asyncio.coroutine
    def coro():
      transport, proto = yield from self.app.loop.create_connection(
          lambda: protocol.LinkProtocol(self.app), link_spec["host"],
          link_spec["port"])
      server = proto.client
      server.name = server_name
      server.was_proposed = True
      self.app.run_hooks("network.connect", server)

    return asyncio.async(coro(), loop=self.app.loop)

  def all(self):
    for name in self.tree.node:
      yield self.get(name)

  def count(self):
    return len(self.tree)

  def add_neighbor(self, server):
    self.add(server)
    self.link(self.local, server)

  def remove(self, server):
    logger.warn("Lost server link to {} ({})".format(server.name, server.sid))
    self.tree.remove_node(server.name)
    del self.servers_by_sid[server.sid]
    self.app.run_hooks("server.remove", server)

  def get(self, name):
    return self.tree.node[name]["server"]

  def get_by_sid(self, sid):
    return self.servers_by_sid[sid]

  def link(self, origin, target):
    logger.info("Connected {} ({}) -> {} ({})".format(origin.name, origin.sid,
                                                      target.name, target.sid))
    self.tree.add_edge(origin.name, target.name)

  def find_shortest_path(self, target, start=None):
    if start is None:
      start = self.local

    _, *path = networkx.algorithms.shortest_path(self.tree, start.name,
                                                 target.name)

    for name in path:
      yield self.get(name)

  def get_next_hop(self, target, start=None):
    return next(self.find_shortest_path(target, start))

  def neighbors(self):
    for name in self.tree.neighbors(self.local.name):
      yield self.get(name)

  def user_broadcast(self, user, prefix, message):
    for channel in user.channels.values():
      channel.broadcast(user, prefix, message)

  def link_broadcast(self, previous, prefix, message):
    for target in self.neighbors():
      if target is not previous:
        target.send(prefix, message)

  def links(self, source=None):
    if source is None:
      source = self.local

    for origin, target in networkx.bfs_edges(self.tree, source.name):
      yield self.get(origin), self.get(target)
