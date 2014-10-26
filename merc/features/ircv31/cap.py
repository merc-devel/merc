from merc import errors
from merc import feature
from merc import message


class CapFeature(feature.Feature):
  NAME = __name__


install = CapFeature.install


class CapReply(message.Reply):
  NAME = "CAP"
  FORCE_TRAILING = True

  def __init__(self, subcommand, arg):
    self.subcommand = subcommand
    self.arg = arg

  def as_reply_params(self, user):
    return [self.subcommand, self.arg]


@CapFeature.register_command
class Cap(message.Command):
  NAME = "CAP"
  MIN_ARITY = 1

  def __init__(self, subcommand, arg=None, *args):
    self.subcommand = subcommand
    self.arg = arg

  def get_supported_caps(self, server):
    caps = set()
    server.run_hooks("modify_caps", server, caps)
    return caps

  def ls(self, user):
    user.is_negotiating_cap = True
    user.send_reply(CapReply("LS",
                             " ".join(self.get_supported_caps(user.server))))

  def list(self, user):
    user.send_reply(CapReply("LIST", " ".join(user.capabilities)))

  def req(self, user, caps):
    supported_caps = self.get_supported_caps(user.server)

    if not (set(caps) - supported_caps):
      user.capabilities.update(caps)
      user.send_reply(CapReply("ACK", " ".join(caps)))
    else:
      user.send_reply(CapReply("NAK", " ".join(caps)))

  def clear(self, user):
    user.send_reply(CapReply("ACK",
                              " ".join("-" + cap for cap in user.capabilities)))
    user.capabilities.clear()

  def end(self, user):
    user.is_negotiating_cap = False
    if user.is_ready_for_registration:
      user.server.register_user(user)

  def handle_for(self, user, prefix):
    subcommand = self.subcommand.upper()

    if subcommand == "LS":
      self.ls(user)
    if subcommand == "LIST":
      self.list(user)
    elif subcommand == "REQ":
      self.req(user, self.arg.split(" ") if self.arg is not None else [])
    elif subcommand == "CLEAR":
      self.clear(user)
    elif subcommand == "END":
      self.end(user)
