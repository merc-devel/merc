from merc import errors
from merc import feature
from merc import message


class UserHostFeature(feature.Feature):
  pass


install = UserHostFeature


class UserHostReply(message.Reply):
  NAME = "302"
  FORCE_TRAILING = True

  def __init__(self, user_hosts):
    self.user_hosts = user_hosts

  def as_reply_params(self, client):
    return [" ".join(self.user_hosts)]


@UserHostFeature.register_command
class UserHost(message.Command):
  NAME = "USERHOST"
  MIN_ARITY = 1

  def __init__(self, *nicknames):
    self.nicknames = nicknames[:5]

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    user_hosts = []
    for nickname in self.nicknames:
      try:
        user = client.server.get_client(nickname)
      except errors.NoSuchNick:
        pass
      else:
        user_hosts.append("{}={}{}".format(
            nickname, "-" if user.is_away else "+", user.hostmask))

    client.send_reply(UserHostReply(user_hosts))
