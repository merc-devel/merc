import logging

from merc import feature


logger = logging.getLogger(__name__)


class BurstFeature(feature.Feature):
  NAME = __name__


install = BurstFeature.install


@BurstFeature.hook("server.register")
def on_register(app, server):
  logger.info("Sending netburst to {} ({})".format(server.name, server.sid))
  app.run_hooks("network.burst.sid", server)
  app.run_hooks("network.burst.ban", server)
  app.run_hooks("network.burst.euid", server)
  app.run_hooks("network.burst.sjoin", server)
  logger.info("Netburst to {} ({}) complete".format(server.name, server.sid))
