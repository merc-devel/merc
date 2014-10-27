from merc import errors
from merc import feature
from merc import message


class AdminFeature(feature.Feature):
  NAME = __name__


install = AdminFeature.install


class AdminInfo(message.Reply):
  NAME = "256"

  def __init__(self, server):
    self.server = server

  def as_reply_params(self):
    return [self.server, "Administrative info"]

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


@AdminFeature.register_command
class Admin(message.Command):
  NAME = "ADMIN"
  MIN_ARITY = 0

  def __init__(self, server=None, *args):
    self.server = server

  @message.Command.requires_registration
  def handle_for(self, server, user, prefix):
    if self.server is not None and self.server != server.name:
      raise errors.NoSuchServer(self.server)

    user.send_reply(AdminInfo(server.name))
    user.send_reply(AdminLocation(server.admin_location))
    user.send_reply(AdminFineLocation(server.admin_location_fine))
    user.send_reply(AdminEmail(server.admin_name, server.admin_email))
