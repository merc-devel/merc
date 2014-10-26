import itertools
import regex

from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import util


class JoinFeature(feature.Feature):
  NAME = __name__


install = JoinFeature.install


MAX_CHANNEL_NAME_LENGTH = 50

CHANNEL_NAME_REGEX = regex.compile(r"^[#&][^\x00\x07\r\n,: ]*$")


class _Join(message.Command):
  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    target = self.get_target(user)

    for channel_name, key in itertools.zip_longest(self.channel_names,
                                                   self.keys,
                                                   fillvalue=None):
      is_new = False

      if CHANNEL_NAME_REGEX.match(channel_name) is None or \
         len(channel_name) > MAX_CHANNEL_NAME_LENGTH:
        raise errors.NoSuchChannel(channel_name)

      try:
        channel = user.server.get_channel(channel_name)
      except errors.NoSuchNick:
        channel = user.server.new_channel(channel_name)
        is_new = True

      if channel.has_user(target):
        continue

      self.check_can_join(target, channel, key)

      channel.join(target)
      channel.broadcast(None, target.hostmask, Join(channel.name))
      user.server.run_hooks("after_join_channel", user, target, channel)
      if is_new:
        user.server.run_hooks("after_join_new_channel", user, target, channel)


@JoinFeature.register_command
class Join(_Join):
  NAME = "JOIN"
  MIN_ARITY = 1

  def __init__(self, channel_names, keys=None, *args):
    self.channel_names = channel_names.split(",")
    self.keys = keys.split(",") if keys is not None else []

  def check_can_join(self, user, channel, key):
    user.server.run_hooks("check_join_channel", user, channel, key)

  def get_target(self, user):
    return user

  def as_params(self, user):
    params = [",".join(self.channel_names)]
    if self.keys:
      params.append(",".join(self.keys))
    return params


@JoinFeature.register_command
class SAJoin(_Join):
  NAME = "SAJOIN"
  MIN_ARITY = 2

  def __init__(self, nickname, channel_names, *args):
    self.nickname = nickname
    self.channel_names = channel_names.split(",")
    self.keys = []

  def check_can_join(self, user, channel, key):
    return

  def get_target(self, user):
    return user.server.get_user(self.nickname)

  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    user.check_is_irc_operator()
    super().handle_for(user, prefix)


class _Part(message.Command):
  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    target = self.get_target(user)

    for channel_name in self.channel_names:
      try:
        channel = user.server.get_channel(channel_name)
      except errors.NoSuchNick:
        raise errors.NoSuchChannel(channel_name)
      else:
        channel.broadcast(None, target.hostmask,
                          Part(channel.name, self.reason))
        user.server.part_channel(target, channel_name)


@JoinFeature.register_command
class Part(_Part):
  NAME = "PART"
  MIN_ARITY = 1

  def __init__(self, channel_names, reason=None, *args):
    self.channel_names = channel_names.split(",")
    self.reason = reason

  @property
  def FORCE_TRAILING(self):
    return self.reason is not None

  def get_target(self, user):
    return user

  def as_params(self, user):
    params = [",".join(self.channel_names)]
    if self.reason is not None:
      params.append(self.reason)
    return params


@JoinFeature.register_command
class SAPart(_Part):
  NAME = "SAPART"
  MIN_ARITY = 2

  def __init__(self, nickname, channel_names, reason=None, *args):
    self.nickname = nickname
    self.channel_names = channel_names.split(",")
    self.reason = reason

  def get_target(self, user):
    return user.server.get_user(self.nickname)

  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    user.check_is_irc_operator()
    super().handle_for(user, prefix)


@JoinFeature.hook("modify_isupport")
def modify_isupport(server, isupport):
  isupport["CHANTYPES"] = "".join(channel.Channel.CHANNEL_CHARS)
  isupport["CHANNELLEN"] = MAX_CHANNEL_NAME_LENGTH
