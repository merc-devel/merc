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

  def as_reply_params(self):
    return [self.subcommand, self.arg]


@CapFeature.register_command
class Cap(message.Command):
  NAME = "CAP"
  MIN_ARITY = 1

  def __init__(self, subcommand, arg=None, *args):
    self.subcommand = subcommand
    self.arg = arg

  def ls(self, server, user):
    user.is_negotiating_cap = True
    user.send_reply(CapReply("LS",
                             " ".join(server.users.capabilities.keys())))

  def list(self, server, user):
    user.send_reply(CapReply("LIST", " ".join(user.capabilities)))

  def req(self, server, user, caps):
    capabilities = server.users.capabilities

    if set(caps) <= set(capabilities.keys()):
      for cap in caps:
        capabilities[cap](user).set()
      user.send_reply(CapReply("ACK", " ".join(caps)))
    else:
      user.send_reply(CapReply("NAK", " ".join(caps)))

  def clear(self, server, user):
    cleared_caps = []

    for capability_factory in server.users.capabilities.values():
      capability = capability_factory(user)

      if capability.get():
        capability.unset()
        cleared_caps.append(capability.NAME)

    user.send_reply(CapReply("ACK",
                             " ".join("-" + cap for cap in cleared_caps)))

  def end(self, server, user):
    user.is_negotiating_cap = False
    if user.is_ready_for_registration:
      user.register(server)

  def handle_for(self, server, user, prefix):
    subcommand = self.subcommand.upper()

    if subcommand == "LS":
      self.ls(server, user)
    if subcommand == "LIST":
      self.list(server, user)
    elif subcommand == "REQ":
      self.req(server, user,
               self.arg.split(" ") if self.arg is not None else [])
    elif subcommand == "CLEAR":
      self.clear(server, user)
    elif subcommand == "END":
      self.end(server, user)
