from merc import errors
from merc import feature
from merc import message
from merc import util


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
    # TODO: add server links
    pass


@SidFeature.hook("network.burst.servers")
def burst_sids(app, server):
  for source, target in app.network.all_links():
    server.send(source.sid, Sid(target.name, "1", target.sid,
                                target.description))
