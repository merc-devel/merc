from merc import errors
from merc import feature
from merc import message


class ConnectFeature(feature.Feature):
  NAME = __name__


install = ConnectFeature.install


@ConnectFeature.hook("link.connect")
def on_connect(app, server):
  current_server = app.network.current
  app.run_hooks("server.pass", server,
                app.network.get_send_password(server),
                current_server.sid)
  app.run_hooks("server.server", server, current_server.name, "1",
                current_server.description)
  app.run_hooks("server.svinfo", server)
