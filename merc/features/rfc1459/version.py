from merc import errors
from merc import feature
from merc import message


class VersionFeature(feature.Feature):
  NAME = __name__


install = VersionFeature.install


class VersionReply(message.Reply):
  NAME = "351"
  FORCE_TRAILING = True

  def __init__(self, version, server, comment):
    self.version = version
    self.server = server
    self.comment = comment

  def as_reply_params(self, server, user):
    return [self.version, self.server, self.comment]


@VersionFeature.register_command
class Version(message.Command):
  NAME = "VERSION"
  MIN_ARITY = 0

  def __init__(self, server=None):
    self.server = server

  @message.Command.requires_registration
  def handle_for(self, server, user, prefix):
    if self.server and self.server != server.name:
      raise errors.NoSuchServer(self.server)

    version = 'merc-{}'.format(server.version)
    user.send_reply(VersionReply(version, server.name, "..."))
    server.run_hooks("send_isupport", user)
