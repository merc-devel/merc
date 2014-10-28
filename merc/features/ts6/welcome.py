from merc import errors
from merc import feature
from merc import message
from merc import util


class WelcomeFeature(feature.Feature):
  NAME = __name__


install = WelcomeFeature.install


@WelcomeFeature.register_server_command
class Pass(message.Command):
  NAME = "PASS"
  MIN_ARITY = 4

  def __init__(self, password, ts, ts_version, sid):
    self.password = password
    self.ts = ts
    self.ts_version = ts_version
    self.sid = sid

  def handle_for(self, app, server, prefix):
    if server.is_registered:
      return

    if self.ts != "TS" or self.ts_version != "6" or not util.is_sid(self.sid):
      raise errors.LinkError("Non-TS server")

    server.password = self.password
    server.sid = self.sid


@WelcomeFeature.register_server_command
class Server(message.Command):
  NAME = "SERVER"
  MIN_ARITY = 3

  def __init__(self, name, hopcount, description):
    self.name = name
    self.hopcount = hopcount
    self.description = description

  def handle_for(self, app, server, prefix):
    if server.is_registered:
      return

    if server.sid is None:
      raise errors.LinkError("Non-TS server")

    try:
      server.hopcount = int(self.hopcount)
    except ValueError:
      raise errors.LinkError("Hopcount is not valid")

    server.name = self.name
    server.description = self.description

    server.register(app)


@WelcomeFeature.hook("check_server_registration")
def check_server_registration(app, server):
  links = app.config.get("links", {})

  try:
    link = links[server.name]
  except KeyError:
    raise errors.LinkError("Bogus server name")

  if not app.crypt_context.verify(server.password, link["receive_password"]):
    raise errors.LinkError("Bad link password")

  if server.sid in app.network.sids:
    raise errors.LinkError("SID collision")

  print(server)
