from merc import message
from merc import feature


class RehashFeature(feature.Feature):
  NAME = __name__


install = RehashFeature


class Rehashing(message.Reply):
  NAME = "382"
  FORCE_TRAILING = True

  def as_reply_params(self, user):
    return [user.server.config_filename, "Rehashing configuration"]


@RehashFeature.register_command
class Rehash(message.Command):
  NAME = "REHASH"
  MIN_ARITY = 0

  def handle_for(self, user, prefix):
    user.check_is_irc_operator()
    user.send_reply(Rehashing())
    user.server.rehash()
