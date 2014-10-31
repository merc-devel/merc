from merc import errors
from merc import feature
from merc import message


class IsOnFeature(feature.Feature):
  NAME = __name__


install = IsOnFeature.install


@IsOnFeature.register_server_command
class IsOnReply(message.Reply):
  NAME = "303"
  FORCE_TRAILING = True
  MIN_ARITY = 1

  def __init__(self, nicknames, *args):
    self.nicknames = nicknames

  def as_reply_params(self):
    return [self.nicknames]


@IsOnFeature.register_user_command
class IsOn(message.Command):
  NAME = "ISON"
  MIN_ARITY = 0

  def __init__(self, *nicknames):
    self.nicknames = nicknames

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    is_on = []
    for nickname in self.nicknames:
      try:
        app.users.get(nickname)
      except errors.NoSuchNick:
        pass
      else:
        is_on.append(nickname)

    user.send_reply(IsOnReply(" ".join(is_on)))
