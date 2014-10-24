from merc import errors
from merc import message


class AdminInfo(message.Reply):
  NAME = "256"

  def __init__(self, server):
    self.server = server

  def as_reply_params(self, client):
    return [self.server, "Administrative info"]

class AdminLocation(message.Reply):
  NAME = "257"
  FORCE_TRAILING = True

  def __init__(self, location):
    self.location = location

  def as_reply_params(self, client):
    return [self.location]

class AdminFineLocation(message.Reply):
  NAME = "258"
  FORCE_TRAILING = True

  def __init__(self, location):
    self.location = location

  def as_reply_params(self, client):
    return [self.location]

class AdminEmail(message.Reply):
  NAME = "259"
  FORCE_TRAILING = True

  def __init__(self, name, email):
    self.name = name
    self.email = email

  def as_reply_params(self, client):
    if self.name:
      email = '{} <{}>'.format(self.name, self.email)
    else:
      email = self.email
    return [email]


@message.Command.register
class Admin(message.Command):
  NAME = "ADMIN"
  MIN_ARITY = 0

  def __init__(self, server=None, *args):
    self.server = server

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    if self.server and self.server != client.server.name:
      raise errors.NoSuchServer(self.server)
    else:
      server = client.server

    client.send_reply(AdminInfo(server.name))
    client.send_reply(AdminLocation(server.admin_location))
    client.send_reply(AdminFineLocation(server.admin_location_fine))
    client.send_reply(AdminEmail(server.admin_name, server.admin_email))

