from merc import errors
from merc import feature
from merc import message
from merc import util


class UserHostFeature(feature.Feature):
  NAME = __name__


install = UserHostFeature.install


class UserHostReply(message.Reply):
  NAME = "302"
  FORCE_TRAILING = True

  def __init__(self, user_hosts):
    self.user_hosts = user_hosts

  def as_reply_params(self):
    return [" ".join("{}{}={}{}".format(user_host.nickname,
                                        "*" if user_host.is_oper else "",
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
  def handle_for(self, server, user, prefix):
    user_hosts = []
    for nickname in self.nicknames:
      try:
        target = server.users.get(nickname)
      except errors.NoSuchNick:
        pass
      else:
        user_host = util.Expando(nickname=nickname, is_away=False,
                                 is_oper=target.is_irc_operator,
                                 hostmask=target.hostmask)
        server.run_hooks("modify_userhost_entry", target, user_host)
        user_hosts.append(user_host)

    user.send_reply(UserHostReply(user_hosts))
