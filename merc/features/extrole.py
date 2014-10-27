from merc import feature
from merc import mode


ROLE_CHARS = "~&@%+"
ROLE_MODES = "qaohv"


class ExtRoleFeature(feature.Feature):
  NAME = __name__


install = ExtRoleFeature.install


@ExtRoleFeature.register_channel_mode
class Owner(mode.ChannelRoleMode):
  CHAR = "q"

  def check_for_target(self, user, target):
    self.target.check_is_owner(user)

  def toggle_for_target(self, target):
    target.is_owner = not target.is_owner

  def get_for_target(self, target):
    return target.is_owner


@ExtRoleFeature.register_channel_mode
class Admin(mode.ChannelRoleMode):
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


@ExtRoleFeature.register_channel_mode
class HalfOp(mode.ChannelRoleMode):
  CHAR = "h"

  def toggle_for_target(self, target):
    target.is_halfop = not target.is_halfop

  def get_for_target(self, target):
    return target.is_halfop


@ExtRoleFeature.hook("modify_isupport")
def modify_isupport(app, isupport):
  isupport["PREFIX"] = "(qaohv)~&@%+"
