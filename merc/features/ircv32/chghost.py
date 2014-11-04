from merc import capability
from merc import feature
from merc import message


class ChgHostFeature(feature.Feature):
  NAME = __name__


install = ChgHostFeature.install


@ChgHostFeature.register_user_capability
class ChgHostCapability(capability.Capability):
  NAME = "chghost"


class _ChgHost(message.Command):
  def handle_for(self, app, user, prefix):
    user.check_is_irc_operator()

    target = self.get_target(app, user)
    old_hostmask = target.hostmask

    target.username = self.username
    target.host = self.host
    app.network.user_broadcast(target, old_hostmask,
                               ChgHost(self.username, self.host))


@ChgHostFeature.register_user_command
class ChgHost(_ChgHost):
  NAME = "CHGHOST"
  MIN_ARITY = 2

  def __init__(self, username, host, *args):
    self.username = username
    self.host = host

  def as_command_params(self):
    return [self.username, self.host]

  def can_send_to(self, user):
    return ChgHostCapability(user).get()

  def get_target(self, app, user):
    return user


@ChgHostFeature.register_user_command
class SAChgHost(_ChgHost):
  NAME = "SACHGHOST"
  MIN_ARITY = 3

  def __init__(self, target, username, host, *args):
    self.target = target
    self.username = username
    self.host = host

  def get_target(self, app, user):
    return app.users.get(self.target)
