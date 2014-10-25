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

  def toggle_for_user(self, user):
    raise NotImplementedError

  def get_for_user(self, user):
    raise NotImplementedError

  def set(self, client, value):
    user = self.target.get_channel_user_for(client.server.get_client(value))

    if self.get_for_user(user):
      return False

    self.toggle_for_user(user)
    return True

  def unset(self, client, value):
    user = self.target.get_channel_user_for(client.server.get_client(value))

    if not self.get_for_user(user):
      return False

    self.toggle_for_user(user)
    return True

  def get(self):
    return None


@RolesFeature.register_channel_mode
class Owner(ChannelRoleMode):
  CHAR = "q"

  def toggle_for_user(self, user):
    user.is_owner = not user.is_owner

  def get_for_user(self, user):
    return user.is_owner


@RolesFeature.register_channel_mode
class Admin(ChannelRoleMode):
  CHAR = "a"

  def toggle_for_user(self, user):
    user.is_admin = not user.is_admin

  def get_for_user(self, user):
    return user.is_admin


@RolesFeature.register_channel_mode
class Operator(ChannelRoleMode):
  CHAR = "o"

  def toggle_for_user(self, user):
    user.is_operator = not user.is_operator

  def get_for_user(self, user):
    return user.is_operator


@RolesFeature.register_channel_mode
class HalfOp(ChannelRoleMode):
  CHAR = "h"

  def toggle_for_user(self, user):
    user.is_halfop = not user.is_halfop

  def get_for_user(self, user):
    return user.is_halfop


@RolesFeature.register_channel_mode
class Voiced(ChannelRoleMode):
  CHAR = "v"

  def toggle_for_user(self, user):
    user.is_voiced = not user.is_voiced

  def get_for_user(self, user):
    return user.is_voiced
