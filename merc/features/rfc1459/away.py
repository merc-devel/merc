from merc import errors
from merc import feature
from merc import message


class AwayFeature(feature.Feature):
  NAME = __name__


install = AwayFeature.install


class IsAway(message.Reply):
  NAME = "301"
  FORCE_TRAILING = True

  def __init__(self, nick, msg):
    self.nick = nick
    self.msg = msg

  def as_reply_params(self, user):
    return [self.nick, self.msg]


class NowAway(message.Reply):
  NAME = "306"

  def as_reply_params(self, user):
    return ["You have been marked as being away"]


class UnAway(message.Reply):
  NAME = "305"

  def as_reply_params(self, user):
    return ["You are no longer marked as being away"]


@AwayFeature.register_command
class Away(message.Command):
  NAME = "AWAY"
  MIN_ARITY = 0

  def __init__(self, message=None, *args):
    self.message = message

  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    locals = user.get_feature_locals(AwayFeature)
    locals["away"] = self.message

    if self.message:
      user.send_reply(NowAway())
    else:
      user.send_reply(UnAway())


@AwayFeature.hook("after_user_invite")
@AwayFeature.hook("after_user_privmsg")
@AwayFeature.hook("after_user_whois")
def send_is_away_if_away(user, target):
  locals = target.get_feature_locals(AwayFeature)

  if locals.get("away", None) is not None:
    user.send_reply(IsAway(target.nickname, locals["away"]))


@AwayFeature.hook("modify_who_reply")
def modify_who_reply(user, target, reply):
  locals = target.user.get_feature_locals(AwayFeature)
  reply.is_away = locals.get("away", None) is not None


@AwayFeature.hook("modify_userhost_entry")
def modify_userhost_reply(target, entry):
  locals = target.get_feature_locals(AwayFeature)
  entry.is_away = locals.get("away", None) is not None
