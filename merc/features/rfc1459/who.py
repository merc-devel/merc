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

  def __init__(self, target):
    self.target = target

  def as_reply_params(self):
    return [self.target, "End of /WHO command"]


class WhoReply(message.Reply):
  NAME = "352"
  FORCE_TRAILING = True

  def __init__(self, target, is_away, multi_prefix):
    self.target = target
    self.is_away = is_away
    self.multi_prefix = multi_prefix

  def as_reply_params(self):
    return [self.target.channel.name if self.target.channel is not None
                                     else "*",
            self.target.user.username, self.target.user.host,
            self.target.user.server_name, self.target.user.nickname,
            ("H" if not self.is_away else "G") +
                (self.target.sigils if self.multi_prefix
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
      reply = WhoReply(cu, False, False)
      app.run_hooks("modify_who_reply", user, cu, reply)
      user.send_reply(reply)

    user.send_reply(EndOfWho(self.target))
