from merc import errors
from merc import feature
from merc import message


class VersionFeature(feature.Feature):
  NAME = __name__


install = VersionFeature.install


class VersionReply(message.Reply):
  NAME = "351"
  FORCE_TRAILING = True

  def __init__(self, version, server_name, link_info, link_protocol, sid):
    self.version = version
    self.server_name = server_name
    self.link_info = link_info
    self.link_protocol = link_protocol
    self.sid = sid

  def as_reply_params(self):
    return [self.version, self.server_name, "{} {} {}".format(
        "".join(sorted(self.link_info, key=lambda c: c.lower()))
            if self.link_info is not None else "",
        self.link_protocol if self.link_protocol is not None else "",
        self.sid)]


@VersionFeature.register_user_command
class Version(message.Command):
  NAME = "VERSION"
  MIN_ARITY = 0

  def __init__(self, server_name=None):
    self.server_name = server_name

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    if self.server_name is not None and self.server_name != app.server_name:
      raise errors.NoSuchServer(self.server_name)

    version = 'merc-{}'.format(app.version)
    reply = VersionReply(version, app.server_name, set(["6"]), None, app.sid)
    app.run_hooks("server.version.modify", reply)
    user.send_reply(reply)
    app.run_hooks("server.isupport.send", user)
