from merc import errors
from merc import feature
from merc import message


class IsOnFeature(feature.Feature):
  pass


install = IsOnFeature


class IsOnReply(message.Reply):
  NAME = "303"
  FORCE_TRAILING = True

  def __init__(self, nicknames):
    self.nicknames = nicknames

  def as_reply_params(self, client):
    return [" ".join(self.nicknames)]


@IsOnFeature.register_command
class IsOn(message.Command):
  NAME = "ISON"
  MIN_ARITY = 0

  def __init__(self, *nicknames):
    self.nicknames = nicknames

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    is_on = []
    for nickname in self.nicknames:
      try:
        client.server.get_client(nickname)
      except errors.NoSuchNick:
        pass
      else:
        is_on.append(nickname)

    client.send_reply(IsOnReply(is_on))
