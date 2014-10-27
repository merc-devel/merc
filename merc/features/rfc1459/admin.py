from merc import errors
from merc import feature
from merc import message


class AdminFeature(feature.Feature):
  NAME = __name__


install = AdminFeature.install


class AdminInfo(message.Reply):
  NAME = "256"

  def __init__(self, app):
    self.app = app

  def as_reply_params(self):
    return [self.app, "Administrative info"]

class AdminLocation(message.Reply):
  NAME = "257"
  FORCE_TRAILING = True

  def __init__(self, location):
    self.location = location

  def as_reply_params(self):
    return [self.location]

class AdminFineLocation(message.Reply):
  NAME = "258"
  FORCE_TRAILING = True

  def __init__(self, location):
    self.location = location

  def as_reply_params(self):
    return [self.location]

class AdminEmail(message.Reply):
  NAME = "259"
  FORCE_TRAILING = True

  def __init__(self, name, email):
    self.name = name
    self.email = email

  def as_reply_params(self):
    if self.name:
      email = '{} <{}>'.format(self.name, self.email)
    else:
      email = self.email
    return [email]


@AdminFeature.register_user_command
class Admin(message.Command):
  NAME = "ADMIN"
  MIN_ARITY = 0

  def __init__(self, app=None, *args):
    self.app = app

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    if self.app is not None and self.app != app.name:
      raise errors.NoSuchServer(self.app)

    user.send_reply(AdminInfo(app.name))
    user.send_reply(AdminLocation(app.admin_location))
    user.send_reply(AdminFineLocation(app.admin_location_fine))
    user.send_reply(AdminEmail(app.admin_name, app.admin_email))
