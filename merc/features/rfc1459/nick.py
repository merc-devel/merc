import regex

from merc import errors
from merc import feature
from merc import message


MAX_NICKNAME_LENGTH = 12
NICKNAME_REGEX = regex.compile(r"^[\p{L}\p{So}_\[\]\\^{}|`][\p{L}\p{So}\p{N}_\[\]\\^{}|`-]*$")


class NickFeature(feature.Feature):
  NAME = __name__


install = NickFeature.install


class _Nick(message.Command):
  def handle_for(self, app, user, prefix):
    target = self.get_target(app, user)
    old_hostmask = target.hostmask

    if NICKNAME_REGEX.match(self.nickname) is None or \
        len(self.nickname) > MAX_NICKNAME_LENGTH:
      raise errors.ErroneousNickname

    app.users.rename(target, self.nickname)

    if target.is_registered:
      target.relay_to_all(Nick(self.nickname), old_hostmask)
      target.send(old_hostmask, Nick(self.nickname))
    else:
      if target.is_ready_for_registration:
        target.register(app)


@NickFeature.register_user_command
class Nick(_Nick):
  NAME = "NICK"
  MIN_ARITY = 1

  def __init__(self, nickname, *args):
    self.nickname = nickname

  def as_command_params(self):
    return [self.nickname]

  def get_target(self, app, user):
    return user


@NickFeature.register_user_command
class SANick(_Nick):
  NAME = "SANICK"
  MIN_ARITY = 2

  def __init__(self, target, nickname, *args):
    self.target = target
    self.nickname = nickname

  def get_target(self, app, user):
    return app.users.get(self.target)

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    user.check_is_irc_operator()
    super().handle_for(app, user, prefix)


@NickFeature.hook("server.isupport.modify")
def modify_isupport(app, isupport):
  isupport["NICKLEN"] = MAX_NICKNAME_LENGTH
