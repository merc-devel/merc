from merc import errors
from merc import feature
from merc import message


class KickFeature(feature.Feature):
  NAME = __name__


install = KickFeature.install


MAX_TARGETS = 1


@KickFeature.register_user_command
class Kick(message.Command):
  NAME = "KICK"
  MIN_ARITY = 2

  def __init__(self, channel_name, nickname, reason=None):
    self.channel_name = channel_name
    self.nickname = nickname

    self.reason = reason

  @property
  def FORCE_TRAILING(self):
    return self.reason is not None

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    try:
      channel = app.channels.get(self.channel_name)
    except errors.NoSuchNick:
      raise errors.NoSuchChannel(self.channel_name)

    target = app.users.get(self.nickname)

    channel.check_has_user(user)
    channel.check_has_user(target)
    channel.check_is_halfop(user)

    channel.broadcast(None, user.hostmask,
                      Kick(self.channel_name, self.nickname, self.reason))
    channel.part(target)

  def as_command_params(self):
    params = [self.channel_name, self.nickname]
    if self.reason is not None:
      params.append(self.reason)
    return params


@KickFeature.hook("server.targmax.modify")
def modify_targmax(app, targmax):
  targmax["KICK"] = MAX_TARGETS
