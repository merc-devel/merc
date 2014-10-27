from merc import errors
from merc import feature
from merc import message
from merc import mode

INVITE_EXPIRATION_TIME = 300


class InviteFeature(feature.Feature):
  NAME = __name__


install = InviteFeature.install


class InviteOnlyChannel(errors.ParametrizedError):
  NAME = "473"
  REASON = "Cannot join channel (+i)"


class Inviting(message.Reply):
  NAME = "341"

  def __init__(self, channel, nick):
    self.channel = channel
    self.nick = nick

  def as_reply_params(self):
    return [self.channel, self.nick]


@InviteFeature.register_channel_mode
class InviteOnly(mode.FlagMode, mode.ChanModeMixin):
  CHAR = "i"


@InviteFeature.register_user_command
class Invite(message.Command):
  NAME = "INVITE"
  MIN_ARITY = 2

  def __init__(self, target, channel):
    self.target = target
    self.channel = channel

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    target = app.users.get(self.target)
    channel = app.channels.get(self.channel)

    channel_user = channel.get_channel_user_for(user)
    if InviteOnly(channel).get() and not channel_user.is_operator_equivalent:
      raise errors.ChanOpPrivsNeeded(self.channel)

    try:
      channel.get_channel_user_for(target)
    except errors.NotOnChannel:
      pass
    else:
      raise errors.AlreadyOnChannel(self.target, self.channel)

    locals = target.get_feature_locals(InviteFeature)
    locals.setdefault("invited_channels", set())
    locals["invited_channels"].add(channel)

    # Set timer to expire invite.
    app.loop.call_later(INVITE_EXPIRATION_TIME,
                           lambda: locals["invited_channels"].discard(channel))

    target.send(user.hostmask, Invite(self.target, self.channel))
    user.send_reply(Inviting(self.channel, self.target))
    app.run_hooks("after_user_invite", user, target)

  def as_command_params(self):
    return [self.target, self.channel]


@InviteFeature.hook("check_join_channel")
def check_invite_status(app, target, channel, key):
  if InviteOnly(channel).get():
    locals = target.get_feature_locals(InviteFeature)
    invites = locals.get("invited_channels", set())

    if channel not in invites:
      raise InviteOnlyChannel(channel.name)
    else:
      invites.discard(channel)
