import networkx
import networkx.algorithms

from merc import errors


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

  @property
  def displayed_nickname(self):
    return ""

  def send(self, prefix, msg):
    self.protocol.send(prefix, msg)

  def on_connect(self, app):
    return
    self.protocol.close("Server connections not supported yet")

  def on_raw_message(self, app, prefix, command_name, params):
    try:
      command_type = app.get_server_command(command_name)
    except KeyError:
      host, *_ = self.protocol.transport.get_extra_info("peername")
      self.send(None,
                errors.Error("Closing Link: {} (Unknown command: {})".format(
                    host, command_name)))
      self.protocol.close()
    else:
      self.on_message(app, prefix, command_type.with_params(params))

  def on_message(self, app, prefix, message):
    message.handle_for(app, self, prefix)

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
