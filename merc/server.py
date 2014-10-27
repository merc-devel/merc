import networkx


class Server(object):
  def __init__(self, store):
    self.store = store


class Neighbor(Server):
  def __init__(self, store, protocol):
    super().__init__(store)
    self.protocol = protocol

  def on_connect(self, app):
    self.protocol.close("Server connections not supported yet")

  def on_disconnect(self, exc):
    pass


class Network(object):
  def __init__(self, app):
    self.app = app
    self.tree = networkx.Graph()

  def new_neighbor(self, protocol):
    return Neighbor(self, protocol)
