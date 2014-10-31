from merc import errors
from merc import feature
from merc import message


class AwayFeature(feature.Feature):
  NAME = __name__


install = AwayFeature.install


MAX_AWAY_LENGTH = 200


class IsAway(message.Reply):
  NAME = "301"
  FORCE_TRAILING = True

  def __init__(self, nick, msg):
    self.nick = nick
    self.msg = msg

  def as_reply_params(self):
    return [self.nick, self.msg]


class NowAway(message.Reply):
  NAME = "306"

  def __init__(self, reason="You have been marked as being away", *args):
    self.reason = reason

  def as_reply_params(self):
    return [self.reason]


class UnAway(message.Reply):
  NAME = "305"

  def __init__(self, reason="You are no longer marked as being away", *args):
    self.reason = reason

  def as_reply_params(self):
    return [self.reason]


@AwayFeature.register_user_command
class Away(message.Command):
  NAME = "AWAY"
  MIN_ARITY = 0

  def __init__(self, message=None, *args):
    self.message = message

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    locals = user.get_feature_locals(AwayFeature)
    locals["away"] = self.message[:MAX_AWAY_LENGTH] \
                     if self.message is not None else None

    if self.message:
      user.send_reply(NowAway())
    else:
      user.send_reply(UnAway())


@AwayFeature.hook("channel.invite")
@AwayFeature.hook("user.whois")
def send_is_away_if_away(app, user, target):
  locals = target.get_feature_locals(AwayFeature)

  if locals.get("away", None) is not None:
    user.send_reply(IsAway(target.nickname, locals["away"]))

@AwayFeature.hook("user.message")
def send_is_away_if_away_message(app, user, target, message):
  locals = target.get_feature_locals(AwayFeature)

  if locals.get("away", None) is not None:
    user.send_reply(IsAway(target.nickname, locals["away"]))

@AwayFeature.hook("server.who.modify")
def modify_who_reply(app, user, target, reply):
  locals = target.user.get_feature_locals(AwayFeature)
  reply.is_away = locals.get("away", None) is not None


@AwayFeature.hook("server.userhost.modify")
def modify_userhost_reply(app, target, entry):
  locals = target.get_feature_locals(AwayFeature)
  entry.is_away = locals.get("away", None) is not None


@AwayFeature.hook("server.isupport.modify")
def modify_isupport(app, isupport):
  isupport["AWAYLEN"] = MAX_AWAY_LENGTH
