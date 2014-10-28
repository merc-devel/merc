import networkx
import networkx.algorithms

from merc import errors


class Server(object):
  def __init__(self, network):
    self.network = network


class CurrentServer(Server):
  def __init__(self, network, app):
    super().__init__(network)
    self.app = app

  @property
  def name(self):
    return self.app.server_name

  @property
  def sid(self):
    return self.app.sid


class Neighbor(Server):
  def __init__(self, network, protocol):
    super().__init__(network)
    self.protocol = protocol

    self.name = None
    self.password = None
    self.sid = None
    self.hopcount = None

    self.is_registered = False

  def register(self, app):
    app.run_hooks("check_server_registration", self)
    try:
      self.network.add_neighbor(self)
    except KeyError:
      raise errors.LinkError("Name collision")
    self.is_registered = True
    app.run_hooks("after_server_register", self)

  @property
  def displayed_nickname(self):
    return ""

  def send(self, prefix, msg):
    self.protocol.send(prefix, msg)

  def on_connect(self, app):
    pass

  def on_raw_message(self, app, prefix, command_name, params):
    try:
      command_type = app.get_server_command(command_name)
    except KeyError:
      host, *_ = self.protocol.transport.get_extra_info("peername")
      self.send(None,
                errors.LinkError("Unknown command: {}".format(command_name)))
      self.protocol.close()
    else:
      try:
        self.on_message(app, prefix, command_type.with_params(params))
      except errors.Error as e:
        self.send(None, e)
        self.protocol.close()
      except errors.BaseError as e:
        self.send(None, errors.LinkError(e.REASON))
        self.protocol.close()

  def on_message(self, app, prefix, message):
    message.handle_for(app, self, prefix)

  def on_disconnect(self, exc):
    if self.is_registered:
      self.network.remove(self)


class NonNeighbor(Server):
  # alternative name: ThatOneOddFamilyMemberYouNeverBotheredWithBecauseTheySeemWeirdButNotWeirdInTheGoodWay
  def __init__(self, network, name, sid):
    super().__init__(network)
    self.name = name
    self.sid = sid


class Network(object):
  def __init__(self, app):
    self.app = app
    self.tree = networkx.Graph()

    self.current = CurrentServer(self, app)
    self.add(self.current)

  @property
  def name(self):
    return self.app.network_name

  @property
  def sids(self):
    return {server.sid for server in self.all()}

  def new_neighbor(self, protocol):
    return Neighbor(self, protocol)

  def new_non_neighbor(self, name, sid):
    return NonNeighbor(self, name, sid)

  def add(self, server):
    if server.name in self.tree.node:
      raise KeyError(server.name)

    self.tree.add_node(server.name, {"server": server})

  def all(self):
    for name in self.tree.node:
      yield self.get(name)

  def add_neighbor(self, server):
    self.add(server)
    self.link(self.current, server)

  def remove(self, server):
    self.tree.remove_node(server.name)
    self.app.run_hooks("after_remove_server", server)

  def get(self, name):
    return self.tree.node[name]["server"]

  def link(self, origin, target):
    self.tree.add_edge(origin.name, target.name)

  def find_shortest_path(self, target, start=None):
    if start is None:
      start = self.current

    for name in networkx.algorithms.shortest_path(self.tree, start.name,
                                                  target.name):
      yield self.get(name)

  def neighborhood(self):
    for name in self.tree.neighbors(self.current.name):
      yield self.get(name)

  def multicast_to_neighbors(self, prefix, message):
    for neighbor in self.neighborhood():
      neighbor.send(prefix, message)
