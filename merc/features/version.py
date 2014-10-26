from merc import util
from merc import feature
from merc import message
from merc.features import welcome


class VersionFeature(feature.Feature):
  NAME = __name__


install = VersionFeature


class VersionReply(message.Reply):
  NAME = "351"
  FORCE_TRAILING = True

  def __init__(self, version, server, comment):
    self.version = version
    self.server = server
    self.comment = comment

  def as_reply_params(self, user):
    return [self.version, self.server, self.comment]


@VersionFeature.register_command
class Version(message.Command):
  NAME = "VERSION"
  MIN_ARITY = 0

  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    version = 'merc-{}'.format(util.get_version())

    user.send_reply(VersionReply(version, user.server.name, "..."))
    user.send_reply(welcome.ISupport(user.server.isupport))
