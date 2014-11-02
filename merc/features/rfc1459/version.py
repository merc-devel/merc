from merc import errors
from merc import feature
from merc import message
from merc import util


class VersionFeature(feature.Feature):
  NAME = __name__


install = VersionFeature.install


@VersionFeature.register_server_command
class VersionReply(message.Reply):
  NAME = "351"
  FORCE_TRAILING = True
  MIN_ARITY = 3

  def __init__(self, version, server_name, description, *args):
    self.version = version
    self.server_name = server_name
    self.description = description

  def as_reply_params(self):
    return [self.version, self.server_name, self.description]


@VersionFeature.register_user_command
class Version(message.Command):
  NAME = "VERSION"
  MIN_ARITY = 0

  def __init__(self, server_name=None):
    self.server_name = server_name

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    if self.server_name is not None and self.server_name != app.server.name:
      raise errors.NoSuchServer(self.server_name)

    reply = util.Expando(version="merc-{}".format(app.version),
                         server_name=app.server.name,
                         link_info={"6"}, link_protocol=None, sid=app.network.local.sid)
    app.run_hooks("server.version.modify", reply)

    user.send_reply(VersionReply(
        reply.version, reply.server_name,
        "{} {} {}".format(
            "".join(sorted(reply.link_info, key=lambda c: c.lower()))
                if reply.link_info is not None else "",
            reply.link_protocol if reply.link_protocol is not None else "",
            reply.sid)))
    app.run_hooks("server.isupport.send", user)
