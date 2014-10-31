from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import util


class WhoFeature(feature.Feature):
  NAME = __name__


install = WhoFeature.install


class EndOfWho(message.Reply):
  NAME = "315"
  FORCE_TRAILING = True

  def __init__(self, target, reason="End of /WHO command", *args):
    self.target = target
    self.reason = reason

  def as_reply_params(self):
    return [self.target, self.reason]


class WhoReply(message.Reply):
  NAME = "352"
  FORCE_TRAILING = True

  def __init__(self, channel_name, username, host, server_name, nickname,
               sigils, realname, *args):
    self.channel_name = channel_name
    self.username = username
    self.host = host
    self.server_name = server_name
    self.nickname = nickname
    self.sigils = sigils
    self.realname = realname

  def as_reply_params(self):
    return [self.channel_name, self.username, self.host, self.server_name,
            self.nickname, self.sigils, self.realname]


@WhoFeature.register_user_command
class Who(message.Command):
  NAME = "WHO"
  MIN_ARITY = 1

  def __init__(self, target, query_type=None, *args):
    self.target = target
    self.query_type = query_type

  def user_matches_query_type(self, target):
    if self.query_type == "o":
      return target.is_irc_operator
    return True

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    who = []

    try:
      if channel.Channel.is_channel_name(self.target):
        chan = app.channels.get(self.target)

        if user.can_see_channel(chan):
          who = [target for target in chan.get_visible_users_for(user)
                        if self.user_matches_query_type(target.user)]
      else:
        for target in app.users.query(self.target):
          if target.is_invisible and target.nickname != self.target:
            continue

          if not self.user_matches_query_type(target):
            continue

          try:
            visible_channel = next(iter(user.get_channels_visible_for(target)))
          except StopIteration:
            visible_channel = None

          if visible_channel is None:
            who.append(channel.ChannelUser(None, target))
          else:
            who.append(visible_channel.get_channel_user_for(target))
    except errors.NoSuchNick:
      pass

    for cu in who:
      reply = util.Expando(target=cu, is_away=False, multi_prefix=False)
      app.run_hooks("server.who.modify", user, cu, reply)
      user.send_reply(WhoReply(
          reply.target.channel.name if reply.target.channel is not None
                                    else "*",
          reply.target.user.username, reply.target.user.host,
          reply.target.user.server_name, reply.target.user.nickname,
          ("H" if not reply.is_away else "G") +
              (reply.target.sigils if reply.multi_prefix
                                   else reply.target.sigil),
          str(0) + " " + reply.target.user.realname))

    user.send_reply(EndOfWho(self.target))
