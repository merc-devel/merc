from merc import errors
from merc import feature
from merc import message


class KickFeature(feature.Feature):
  NAME = __name__


install = KickFeature.install


MAX_TARGETS = 1


@KickFeature.register_command
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
  def handle_for(self, user, prefix):
    try:
      channel = user.server.channels.get(self.channel_name)
    except errors.NoSuchNick:
      raise errors.NoSuchChannel(self.channel_name)

    target = channel.server.users.get(self.nickname)

    channel.check_has_user(user)
    channel.check_has_user(target)
    channel.check_is_halfop(user)

    channel.broadcast(None, user.hostmask,
                      Kick(self.channel_name, self.nickname, self.reason))
    channel.part(target)


  def as_params(self, user):
    params = [self.channel_name, self.nickname]
    if self.reason is not None:
      params.append(self.reason)
    return params


@KickFeature.hook("modify_targmax")
def modify_targmax(targmax):
  targmax["KICK"] = MAX_TARGETS
