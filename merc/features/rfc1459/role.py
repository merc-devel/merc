from merc import feature
from merc import mode


class RoleFeature(feature.Feature):
  NAME = __name__


install = RoleFeature.install


@RoleFeature.register_channel_mode
class Operator(mode.ChannelRoleMode):
  CHAR = "o"

  def toggle_for_target(self, target):
    target.is_operator = not target.is_operator

  def get_for_target(self, target):
    return target.is_operator


@RoleFeature.register_channel_mode
class Voiced(mode.ChannelRoleMode):
  CHAR = "v"

  def toggle_for_target(self, target):
    target.is_voiced = not target.is_voiced

  def get_for_target(self, target):
    return target.is_voiced


@RoleFeature.hook("modify_isupport")
def modify_isupport(server, isupport):
  if "PREFIX" in isupport:
    # We may be overriden by extrole, and we're not sure if extrole was loaded
    # before us -- so if extrole has written to PREFIX already, we won't write
    # to PREFIX.
    return

  isupport["PREFIX"] = "(ov)@+"
