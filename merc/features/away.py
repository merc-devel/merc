from merc import errors
from merc import feature
from merc import message


class AwayFeature(feature.Feature):
  NAME = __name__


install = AwayFeature


class IsAway(message.Reply):
  NAME = "301"
  FORCE_TRAILING = True

  def __init__(self, nick, msg):
    self.nick = nick
    self.msg = msg

  def as_reply_params(self, client):
    return [self.nick, self.msg]


class NowAway(message.Reply):
  NAME = "306"

  def as_reply_params(self, client):
    return ["You have been marked as being away"]


class UnAway(message.Reply):
  NAME = "305"

  def as_reply_params(self, client):
    return ["You are no longer marked as being away"]


@AwayFeature.register_command
class Away(message.Command):
  NAME = "AWAY"
  MIN_ARITY = 0

  def __init__(self, message=None, *args):
    self.message = message

  @message.Command.requires_registration
  def handle_for(self, client, prefix):
    client.away_message = self.message
    if self.message:
      client.send_reply(NowAway())
    else:
      client.send_reply(UnAway())


@AwayFeature.hook("after_new_client")
def set_default_away_message(client):
  client.away_message = None


@AwayFeature.hook("after_user_privmsg")
@AwayFeature.hook("after_user_whois")
def send_is_away_if_away(client, user):
  if user.is_away:
    client.send_reply(away.IsAway(user.nickname, user.away_message))
