from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import util


class WhoFeature(feature.Feature):
  NAME = __name__


install = WhoFeature


class EndOfWho(message.Reply):
  NAME = "315"
  FORCE_TRAILING = True

  def __init__(self, target):
    self.target = target

  def as_reply_params(self, user):
    return [self.target, "End of /WHO command"]


class WhoReply(message.Reply):
  NAME = "352"
  FORCE_TRAILING = True

  def __init__(self, channel_name, username, host, server, nickname, is_away,
               hopcount, realname):
    self.channel_name = channel_name
    self.username = username
    self.host = host
    self.server = server
    self.nickname = nickname
    self.is_away = is_away
    self.hopcount = hopcount
    self.realname = realname

  def as_reply_params(self, user):
    return [self.channel_name if self.channel_name is not None else "*",
            self.username, self.host, self.server, self.nickname,
            "H" if not self.is_away else "G",
            str(self.hopcount) + " " + self.realname]


@WhoFeature.register_command
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
  def handle_for(self, user, prefix):
    who = []

    try:
      if channel.Channel.is_channel_name(self.target):
        chan = user.server.get_channel(self.target)

        if user.can_see_channel(chan):
          who = [(chan.name, target.user)
                 for target in chan.get_visible_users_for(user)
                 if self.user_matches_query_type(target.user)]
      else:
        for target in user.server.query_users(self.target):
          if not self.user_matches_query_type(target):
            continue

          visible_it = iter(user.get_channels_visible_for(target))

          try:
            visible_channel = next(visible_it)
          except StopIteration:
            visible_channel_name = None
          else:
            visible_channel_name = visible_channel.name

          who.append((visible_channel_name, target))
    except errors.NoSuchNick:
      pass

    for channel_name, target in who:
      reply = WhoReply(channel_name, target.username, target.host,
                       target.server.name, target.nickname, False, 0,
                       target.realname)
      user.server.run_hooks("modify_who_reply", target, reply)
      user.send_reply(reply)

    user.send_reply(EndOfWho(self.target))
