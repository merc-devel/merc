import asyncio
import logging
import networkx
import networkx.algorithms

from merc import errors
from merc import protocol
from merc import util


logger = logging.getLogger(__name__)


class Server(object):
  def __init__(self, network):
    self.network = network

  @property
  def debug_id(self):
    return "server:" + (self.sid if self.sid is not None else "?")


class CurrentServer(Server):
  def __init__(self, network, app):
    super().__init__(network)
    self.app = app

    self.was_proposed = True

  @property
  def name(self):
    return self.app.server_name

  @property
  def description(self):
    return self.app.config["server"]["description"]

  @property
  def sid(self):
    return self.app.sid

  @property
  def hopcount(self):
    return 0


class Neighbor(Server):
  def __init__(self, network, protocol):
    super().__init__(network)
    self.protocol = protocol

    self.name = None
    self.password = None
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
    return ""

  def send(self, prefix, msg, source=None):
    self.protocol.send(prefix, msg, source)

  def on_connect(self, app):
    pass

  def on_raw_message(self, app, prefix, command_name, params):
    if all(c.isdigit() for c in command_name):
      # TODO: handle numerics
      return

    try:
      if prefix is not None and util.is_uid(prefix):
        command_type = app.get_user_command(command_name)
      else:
        command_type = app.get_server_command(command_name)
    except KeyError:
      host, *_ = self.protocol.transport.get_extra_info("peername")
      self.send(None, errors.UnknownCommand(command_name))
    else:
      if prefix is not None and util.is_uid(prefix):
        target = app.users.get_by_uid(prefix)
      else:
        if prefix is not None and util.is_sid(prefix):
          target = app.network.get_by_sid(prefix)
        else:
          target = self

      try:
        self.on_message(app, target, prefix, command_type.with_params(params))
      except errors.Error as e:
        self.send(None, e)
        self.protocol.close(e.REASON)
      except errors.BaseError as e:
        self.send(None, e)

  def on_message(self, app, target, prefix, message):
    message.handle_for(app, target, prefix)

  def on_disconnect(self, exc):
    if self.is_registered:
      self.network.remove(self)


class NonNeighbor(Server):
  # alternative name: ThatOneOddFamilyMemberYouNeverBotheredWithBecauseTheySeemWeirdButNotWeirdInTheGoodWay
  def __init__(self, network, name, sid, hopcount):
    super().__init__(network)
    self.name = name
    self.sid = sid
    self.hopcount = hopcount

  def send(self, prefix, msg, source=None):
    target = self.network.get_next_hop(self.network.get(self.name))
    target.send(prefix, msg, source)


class Network(object):
  def __init__(self, app):
    self.app = app

    self.tree = networkx.Graph()
    self.servers_by_sid = {}

    self.current = CurrentServer(self, app)
    self.add(self.current)

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

  def new_non_neighbor(self, name, sid, hopcount):
    return NonNeighbor(self, name, sid, hopcount)

  def add(self, server):
    if server.name in self.tree.node:
      raise KeyError(server.name)

    self.tree.add_node(server.name, {"server": server})
    self.servers_by_sid[server.sid] = server

  def has(self, name):
    return name in self.tree

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

  def all_links(self):
    for origin, target in self.tree.edges_iter():
      yield (self.get(origin), self.get(target))

  def count(self):
    return len(self.tree)

  def add_neighbor(self, server):
    self.add(server)
    self.link(self.current, server)

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
      start = self.current

    _, *path = networkx.algorithms.shortest_path(self.tree, start.name,
                                                 target.name)

    for name in path:
      yield self.get(name)

  def get_next_hop(self, target, start=None):
    return next(self.find_shortest_path(target, start))

  def neighborhood(self):
    for name in self.tree.neighbors(self.current.name):
      yield self.get(name)

  def multicast_to_neighbors(self, prefix, message):
    for neighbor in self.neighborhood():
      neighbor.send(prefix, message)
