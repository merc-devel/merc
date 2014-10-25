from merc import message
from merc import feature


class RehashFeature(feature.Feature):
  NAME = __name__


install = RehashFeature


@RehashFeature.register_command
class Rehash(message.Command):
  NAME = "REHASH"
  MIN_ARITY = 0

  def handle_for(self, client, prefix):
    client.check_is_irc_operator()
    client.server.run_hooks("send_server_notice", client,
                            "*** Rehashing configuration.")
    client.server.rehash()
    client.server.run_hooks("send_server_notice", client,
                            "*** Rehash complete.")
