from merc import errors
from merc import feature
from merc import message
from merc import util


class UserHostFeature(feature.Feature):
  NAME = __name__


install = UserHostFeature


class UserHostReply(message.Reply):
  NAME = "302"
  FORCE_TRAILING = True

  def __init__(self, user_hosts):
    self.user_hosts = user_hosts

  def as_reply_params(self, user):
    return [" ".join("{}={}{}".format(user_host.nickname,
                                      "+" if not user_host.is_away else "-",
                                      user_host.hostmask)
            for user_host in self.user_hosts)]


@UserHostFeature.register_command
class UserHost(message.Command):
  NAME = "USERHOST"
  MIN_ARITY = 1

  def __init__(self, *nicknames):
    self.nicknames = nicknames[:5]

  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    user_hosts = []
    for nickname in self.nicknames:
      try:
        user = user.server.get_user(nickname)
      except errors.NoSuchNick:
        pass
      else:
        user_host = util.Expando(nickname=nickname, is_away=False,
                                 hostmask=user.hostmask)
        user.server.run_hooks("modify_userhost_entry", user, user_host)
        user_hosts.append(user_host)

    user.send_reply(UserHostReply(user_hosts))
