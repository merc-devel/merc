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


@RolesFeature.register_channel_mode
class Owner(mode.ChannelRoleMode):
  CHAR = "q"

  def mutate(self, user, value):
    user.is_owner = True


@RolesFeature.register_channel_mode
class Admin(mode.ChannelRoleMode):
  CHAR = "a"

  def mutate(self, user, value):
    user.is_admin = True


@RolesFeature.register_channel_mode
class Operator(mode.ChannelRoleMode):
  CHAR = "o"

  def mutate(self, user, value):
    user.is_operator = True


@RolesFeature.register_channel_mode
class HalfOp(mode.ChannelRoleMode):
  CHAR = "h"

  def mutate(self, user, value):
    user.is_halfop = True


@RolesFeature.register_channel_mode
class Voiced(mode.ChannelRoleMode):
  CHAR = "v"

  def mutate(self, user, value):
    user.is_voiced = True
