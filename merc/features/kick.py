from merc import errors
from merc import feature
from merc import message


class KickFeature(feature.Feature):
  NAME = __name__


install = KickFeature


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
  def handle_for(self, client, prefix):
    try:
      channel = client.server.get_channel(self.channel_name)
    except errors.NoSuchNick:
      raise errors.NoSuchChannel(self.channel_name)

    user = client.server.get_client(self.nickname)

    channel.check_has_client(client)
    channel.check_has_client(user)
    channel.check_is_operator(client)

    channel.broadcast(None, client.hostmask,
                      Kick(self.channel_name, self.nickname, self.reason))
    client.server.part_channel(user, channel.name)


  def as_params(self, client):
    params = [self.channel_name, self.nickname]
    if self.reason is not None:
      params.append(self.reason)
    return params

