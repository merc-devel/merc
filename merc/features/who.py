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

  def __init__(self, target, is_away):
    self.target = target
    self.is_away = is_away

  def as_reply_params(self, user):
    return [self.target.channel.name if self.target.channel is not None
                                     else "*",
            self.target.user.username, self.target.user.host,
            self.target.user.server.name, self.target.user.nickname,
            ("H" if not self.is_away else "G") +
                (self.target.sigils if "multi-prefix" in user.capabilities
                                    else self.target.sigil),
            str(0) + " " + self.target.user.realname]


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
          who = [target for target in chan.get_visible_users_for(user)
                        if self.user_matches_query_type(target.user)]
      else:
        for target in user.server.query_users(self.target):
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
      reply = WhoReply(cu, False)
      user.server.run_hooks("modify_who_reply", cu, reply)
      user.send_reply(reply)

    user.send_reply(EndOfWho(self.target))
