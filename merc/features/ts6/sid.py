from merc import errors
from merc import feature
from merc import message


class SidFeature(feature.Feature):
  NAME = __name__


install = SidFeature.install


@SidFeature.register_server_command
class Sid(message.Command):
  NAME = "SID"
  MIN_ARITY = 4

  def __init__(self, server_name, hopcount, sid, description, *args):
    self.server_name = server_name
    self.hopcount = hopcount
    self.sid = sid
    self.description = description

  def as_command_params(self):
    return [self.server_name, self.hopcount, self.sid, self.description]

  def handle_for(self, app, server, prefix):
    if app.network.has_by_sid(self.sid):
      raise errors.LinkError("SID collision")

    origin = app.network.get_by_sid(prefix)

    non_neighbor = app.network.new_non_neighbor(
        self.server_name, int(self.hopcount) + origin.hopcount, self.sid,
        self.description)
    app.network.add(non_neighbor)
    app.network.link(origin, non_neighbor)

    # Broadcast the SID message to further servers.
    app.network.link_broadcast(server, prefix, self)


@SidFeature.hook("network.burst.servers")
def burst_sids(app, server):
  for neighbor in app.network.neighbors():
    if neighbor is server:
      continue
    server.send(app.network.local.sid,
                Sid(neighbor.name, "1", neighbor.sid, neighbor.description))


@SidFeature.hook("server.register")
def on_connect(app, server):
  app.network.link_broadcast(server, app.network.local.sid,
                             Sid(server.name, "1", server.sid,
                                 server.description))
