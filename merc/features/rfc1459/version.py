from merc import errors
from merc import feature
from merc import message


class VersionFeature(feature.Feature):
  NAME = __name__


install = VersionFeature.install


class VersionReply(message.Reply):
  NAME = "351"
  FORCE_TRAILING = True

  def __init__(self, version, server_name, comment):
    self.version = version
    self.server_name = server_name
    self.comment = comment

  def as_reply_params(self):
    return [self.version, self.server_name, self.comment]


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
    user.send_reply(VersionReply(version, app.server_name, "..."))
    app.run_hooks("send_isupport", user)
