from merc import feature
from merc import mode


ROLE_CHARS = "~&@%+"
ROLE_MODES = "qaohv"


class RolesFeature(feature.Feature):
  NAME = __name__

  @property
  def isupport(self):
    return {
        "PREFIX": "({}){}".format(ROLE_MODES, ROLE_CHARS)
    }


install = RolesFeature


class ChannelRoleMode(mode.Mode):
  TAKES_PARAM = True

  def toggle_for_target(self, target):
    raise NotImplementedError

  def get_for_target(self, target):
    raise NotImplementedError

  def check_for_target(self, user, target):
    self.target.check_is_operator(user)

  def check(self, user, value):
    target = self.target.get_channel_user_for(user.server.get_user(value))
    self.check_for_target(user, target)

  def set(self, user, value):
    target = self.target.get_channel_user_for(user.server.get_user(value))

    if self.get_for_target(target):
      return False

    self.toggle_for_target(target)
    return True

  def unset(self, user, value):
    target = self.target.get_channel_user_for(user.server.get_user(value))

    if not self.get_for_target(target):
      return False

    self.toggle_for_target(target)
    return True

  def get(self):
    return None


@RolesFeature.register_channel_mode
class Owner(ChannelRoleMode):
  CHAR = "q"

  def check_for_target(self, user, target):
    self.target.check_is_owner(user)

  def toggle_for_target(self, target):
    target.is_owner = not target.is_owner

  def get_for_target(self, target):
    return target.is_owner


@RolesFeature.register_channel_mode
class Admin(ChannelRoleMode):
  CHAR = "a"

  def check_for_target(self, user, target):
    if target.user is user:
      self.target.check_is_admin(user)
    else:
      self.target.check_is_owner(user)

  def toggle_for_target(self, target):
    target.is_admin = not target.is_admin

  def get_for_target(self, target):
    return target.is_admin


@RolesFeature.register_channel_mode
class Operator(ChannelRoleMode):
  CHAR = "o"

  def toggle_for_target(self, target):
    target.is_operator = not target.is_operator

  def get_for_target(self, target):
    return target.is_operator


@RolesFeature.register_channel_mode
class HalfOp(ChannelRoleMode):
  CHAR = "h"

  def toggle_for_target(self, target):
    target.is_halfop = not target.is_halfop

  def get_for_target(self, target):
    return target.is_halfop


@RolesFeature.register_channel_mode
class Voiced(ChannelRoleMode):
  CHAR = "v"

  def toggle_for_target(self, target):
    target.is_voiced = not target.is_voiced

  def get_for_target(self, target):
    return target.is_voiced
