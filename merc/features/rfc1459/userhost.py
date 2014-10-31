from merc import errors
from merc import feature
from merc import message
from merc import util


class UserHostFeature(feature.Feature):
  NAME = __name__


install = UserHostFeature.install


@UserHostFeature.register_server_command
class UserHostReply(message.Reply):
  NAME = "302"
  FORCE_TRAILING = True
  MIN_ARITY = 1

  def __init__(self, user_hosts, *args):
    self.user_hosts = user_hosts

  def as_reply_params(self):
    return [self.user_hosts]


@UserHostFeature.register_user_command
class UserHost(message.Command):
  NAME = "USERHOST"
  MIN_ARITY = 1

  def __init__(self, *nicknames):
    self.nicknames = nicknames[:5]

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    user_hosts = []
    for nickname in self.nicknames:
      try:
        target = app.users.get(nickname)
      except errors.NoSuchNick:
        pass
      else:
        user_host = util.Expando(nickname=nickname, is_away=False,
                                 is_oper=target.is_irc_operator,
                                 hostmask=target.hostmask)
        app.run_hooks("server.userhost.modify", target, user_host)
        user_hosts.append(user_host)

    user.send_reply(UserHostReply(
        " ".join("{}{}={}{}".format(user_host.nickname,
                                    "*" if user_host.is_oper else "",
                                    "+" if not user_host.is_away else "-",
                                    user_host.hostmask)
                 for user_host in user_hosts)))
