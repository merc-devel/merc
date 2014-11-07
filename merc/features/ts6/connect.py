from merc import feature


class ConnectFeature(feature.Feature):
  NAME = __name__


install = ConnectFeature.install


@ConnectFeature.hook("network.connect")
def on_connect(app, server):
  app.run_hooks("server.pass", server,
                app.network.get_send_password(server),
                app.network.local.sid)
  app.run_hooks("server.server", server, app.network.local.name, "1",
                app.network.local.description)
  app.run_hooks("server.svinfo", server)
