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


@CapFeature.register_user_command
class Cap(message.Command):
  NAME = "CAP"
  MIN_ARITY = 1

  def __init__(self, subcommand, arg=None, *args):
    self.subcommand = subcommand
    self.arg = arg

  def ls(self, app, user):
    user.is_negotiating_cap = True
    user.send_reply(CapReply("LS", " ".join(
        capability.NAME for capability in app.users.capabilities.values())))

  def list(self, app, user):
    user.send_reply(CapReply("LIST", " ".join(
        capability.NAME for capability in app.users.capabilities.values()
                        if capability(user).get())))

  def req(self, app, user, caps):
    if not caps:
      return

    capabilities = app.users.capabilities

    if set(caps) <= set(capabilities.keys()):
      for cap in caps:
        capabilities[cap](user).set()
      user.send_reply(CapReply("ACK", " ".join(caps)))
    else:
      user.send_reply(CapReply("NAK", " ".join(caps)))

  def clear(self, app, user):
    cleared_caps = []

    for capability_factory in app.users.capabilities.values():
      capability = capability_factory(user)

      if capability.get():
        capability.unset()
        cleared_caps.append(capability.NAME)

    user.send_reply(CapReply("ACK",
                             " ".join("-" + cap for cap in cleared_caps)))

  def end(self, app, user):
    user.is_negotiating_cap = False
    if user.is_ready_for_registration:
      user.register(app)

  def handle_for(self, app, user, prefix):
    subcommand = self.subcommand.upper()

    if subcommand == "LS":
      self.ls(app, user)
    if subcommand == "LIST":
      self.list(app, user)
    elif subcommand == "REQ":
      self.req(app, user,
               self.arg.split(" ") if self.arg is not None else [])
    elif subcommand == "CLEAR":
      self.clear(app, user)
    elif subcommand == "END":
      self.end(app, user)
