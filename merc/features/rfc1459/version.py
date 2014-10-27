from merc import errors
from merc import feature
from merc import message


class VersionFeature(feature.Feature):
  NAME = __name__


install = VersionFeature.install


class VersionReply(message.Reply):
  NAME = "351"
  FORCE_TRAILING = True

  def __init__(self, version, app, comment):
    self.version = version
    self.server_name = server_name
    self.comment = comment

  def as_reply_params(self):
    return [self.version, self.server_name, self.comment]


@VersionFeature.register_command
class Version(message.Command):
  NAME = "VERSION"
  MIN_ARITY = 0

  def __init__(self, app=None):
    self.app = app

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    if self.app and self.app != app.name:
      raise errors.NoSuchServer(self.app)

    version = 'merc-{}'.format(app.version)
    user.send_reply(VersionReply(version, app.name, "..."))
    app.run_hooks("send_isupport", user)
