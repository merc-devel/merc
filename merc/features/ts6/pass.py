from merc import errors
from merc import feature
from merc import message
from merc import util


class PassFeature(feature.Feature):
  NAME = __name__


install = PassFeature.install


@PassFeature.register_server_command
class Pass(message.Command):
  NAME = "PASS"
  MIN_ARITY = 4

  def __init__(self, password, ts, ts_version, sid, *args):
    self.password = password
    self.ts = ts
    self.ts_version = ts_version
    self.sid = sid

  def handle_for(self, app, server, prefix):
    if server.is_registered:
      raise errors.LinkError("Server already registered")

    if self.ts != "TS" or self.ts_version != "6" or not util.is_sid(self.sid):
      raise errors.LinkError("Non-TS server")

    server.password = self.password
    server.sid = self.sid

  def as_command_params(self):
    return [self.password, self.ts, self.ts_version, self.sid]


@PassFeature.hook("server.register.check")
def check_server_registration(app, server):
  if server.sid is None:
    raise errors.LinkError("Non-TS server")

  if server.sid in app.network.sids:
    raise errors.LinkError("SID collision")

  links = app.config.get("links", {})

  try:
    link = links[server.name]
  except KeyError:
    raise errors.LinkError("Bogus server name")

  if not app.crypt_context.verify(server.password, link["receive_password"]):
    raise errors.LinkError("Bad link password")


@PassFeature.hook("server.register")
def on_register(app, server):
  if not server.was_proposed:
    app.run_hooks("link.connect", server)


@PassFeature.hook("server.pass")
def send_pass(app, server, password, sid):
  server.send(None, Pass(password, "TS", "6", sid))


@PassFeature.hook("server.version.modify")
def modify_version_link_protocol(app, reply):
  reply.link_protocol = "TS6ow"
