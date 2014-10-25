from merc import feature
from merc import mode


ROLE_CHARS = "~&@%+"
ROLE_MODES = "qaohv"


class RolesFeature(feature.Feature):
  NAME = __name__
  ISUPPORT = {
      "PREFIX": "({}){}".format(ROLE_MODES, ROLE_CHARS)
  }


install = RolesFeature


class ChannelRoleMode(mode.Mode):
  TAKES_PARAM = True

  def mutate(self, user, value):
    raise NotImplementedError

  def set(self, client, value):
    user = self.target.get_channel_user_for(client.server.get_client(value))
    return self.mutate(user, True)

  def unset(self, client, value):
    user = self.target.get_channel_user_for(client.server.get_client(value))
    return self.mutate(user, False)


@RolesFeature.register_channel_mode
class Owner(ChannelRoleMode):
  CHAR = "q"

  def mutate(self, user, value):
    if user.is_owner == value:
      return False

    user.is_owner = value
    return True


@RolesFeature.register_channel_mode
class Admin(ChannelRoleMode):
  CHAR = "a"

  def mutate(self, user, value):
    if user.is_admin == value:
      return False

    user.is_admin = value
    return True


@RolesFeature.register_channel_mode
class Operator(ChannelRoleMode):
  CHAR = "o"

  def mutate(self, user, value):
    if user.is_operator == value:
      return False

    user.is_operator = value
    return True


@RolesFeature.register_channel_mode
class HalfOp(ChannelRoleMode):
  CHAR = "h"

  def mutate(self, user, value):
    if user.is_halfop == value:
      return False

    user.is_halfop = value
    return True


@RolesFeature.register_channel_mode
class Voiced(ChannelRoleMode):
  CHAR = "v"

  def mutate(self, user, value):
    if user.is_voiced == value:
      return False

    user.is_voiced = value
    return True
