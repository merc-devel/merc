import networkx
import networkx.algorithms


class Server(object):
  def __init__(self, store, sid, server_name):
    self.store = store
    self.sid = sid
    self.server_name = server_name


class CurrentServer(Server):
  def __init__(self, store, app):
    super().__init__(store, app.sid, app.name)


class Neighbor(Server):
  def __init__(self, store, protocol):
    super().__init__(store, None, None)
    self.protocol = protocol

  def on_connect(self, app):
    self.protocol.close("Server connections not supported yet")

  def on_disconnect(self, exc):
    pass


class Network(object):
  def __init__(self, app):
    self.app = app
    self.tree = networkx.Graph()

    self.current = CurrentServer(self, app)
    self.add(self.current)

  def new_neighbor(self, protocol):
    return Neighbor(self, protocol)

  def add(self, server):
    self.tree.add_node(server.sid, {"server": server})

  def remove(self, server):
    self.tree.remove_node(server.sid)

  def get(self, sid):
    return self.tree.node[sid]["server"]

  def link(self, origin, target):
    self.tree.add_edge(origin.sid, target.sid)

  def find_shortest_path(self, origin, target):
    for sid in networkx.algorithms.shortest_path(self.tree, origin.sid,
                                                 target.sid):
      yield self.get(sid)
