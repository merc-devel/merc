import datetime

from merc import errors
from merc import feature
from merc import message
from merc import util


class QuitFeature(feature.Feature):
  NAME = __name__


install = QuitFeature.install


@QuitFeature.register_server_command
class Quit(message.Command):
  NAME = "QUIT"
  MIN_ARITY = 0
  FORCE_TRAILING = True

  def __init__(self, reason=None, *args):
    self.reason = reason

  @property
  def sid(self):
    return self.uid[:3]

  @property
  def FORCE_TRAILING(self):
    return self.reason is not None

  @message.Command.requires_registration
  def handle_for(self, app, server, prefix):
    app.users.remove_unsafe(app.users.get_by_uid(prefix))
    app.network.link_broadcast(server, prefix, self)

  def as_command_params(self):
    params = []
    if self.reason is not None:
      params.append(self.reason)
    return params


def send_uid(app, server, user):
  host = user.host
  if host[0] == ":":
    host = "0" + host

  server.send(app.network.local.sid,
              Quit(user.nickname, str(user.hopcount),
                  str(int(user.creation_time.timestamp())), "+", user.username,
                  host, "0", user.uid, user.realname))


@QuitFeature.hook("user.remove.check")
def broadcast_quit_on_quit(app, user):
  app.network.link_broadcast(None, user.link_prefix,
                             Quit(user.protocol.disconnect_reason))
