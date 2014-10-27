from merc import message
from merc import feature


class RehashFeature(feature.Feature):
  NAME = __name__


install = RehashFeature.install


class Rehashing(message.Reply):
  NAME = "382"
  FORCE_TRAILING = True

  def __init__(self, config_filename):
    self.config_filename = config_filename

  def as_reply_params(self):
    return [self.config_filename, "Rehashing configuration"]


@RehashFeature.register_command
class Rehash(message.Command):
  NAME = "REHASH"
  MIN_ARITY = 0

  def handle_for(self, app, user, prefix):
    user.check_is_irc_operator()
    user.send_reply(Rehashing(app.config_filename))
    app.rehash()
