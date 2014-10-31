from merc import message
from merc import feature


class RehashFeature(feature.Feature):
  NAME = __name__


install = RehashFeature.install


@RehashFeature.register_server_command
class Rehashing(message.Reply):
  NAME = "382"
  FORCE_TRAILING = True
  MIN_ARITY = 2

  def __init__(self, config_filename, reason="Rehashing configuration", *args):
    self.config_filename = config_filename
    self.reason = reason

  def as_reply_params(self):
    return [self.config_filename, self.reason]


@RehashFeature.register_user_command
class Rehash(message.Command):
  NAME = "REHASH"
  MIN_ARITY = 0

  def handle_for(self, app, user, prefix):
    user.check_is_irc_operator()
    user.send_reply(Rehashing(app.config_filename))
    app.rehash()
