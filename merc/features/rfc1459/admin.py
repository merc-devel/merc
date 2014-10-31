from merc import errors
from merc import feature
from merc import message


class AdminFeature(feature.Feature):
  NAME = __name__


install = AdminFeature.install


class AdminInfo(message.Reply):
  NAME = "256"

  def __init__(self, server_name, reason="Administrative info", *args):
    self.server_name = server_name
    self.reason = reason

  def as_reply_params(self):
    return [self.server_name, self.reason]


class AdminLocation(message.Reply):
  NAME = "257"
  FORCE_TRAILING = True

  def __init__(self, location, *args):
    self.location = location

  def as_reply_params(self):
    return [self.location]


class AdminFineLocation(message.Reply):
  NAME = "258"
  FORCE_TRAILING = True

  def __init__(self, location, *args):
    self.location = location

  def as_reply_params(self):
    return [self.location]


class AdminEmail(message.Reply):
  NAME = "259"
  FORCE_TRAILING = True

  def __init__(self, email, *args):
    self.email = email

  def as_reply_params(self):
    return [self.email]


@AdminFeature.register_user_command
class Admin(message.Command):
  NAME = "ADMIN"
  MIN_ARITY = 0

  def __init__(self, server_name=None, *args):
    self.server_name = server_name

  def as_command_params(self):
    if self.server_name is not None:
      return [self.server_name]
    return []

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    if self.server_name == app.server_name or self.server_name is None:
      user.send_reply(AdminInfo(app.server_name))
      user.send_reply(AdminLocation(app.admin_location))
      user.send_reply(AdminFineLocation(app.admin_location_fine))

      if app.admin_name:
        email = '{} <{}>'.format(app.admin_name, app.admin_email)
      else:
        email = app.admin_email

      user.send_reply(AdminEmail(email))
      return

    if not app.network.has(self.server_name):
      raise errors.NoSuchServer(self.server_name)

    app.network.get(self.server_name).send(
        prefix if prefix is not None else user.uid, self)
