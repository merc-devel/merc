from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import mode


class NamesFeature(feature.Feature):
  NAME = __name__


install = NamesFeature


MAX_TARGETS = 1


class LUserClient(message.Reply):
  NAME = "251"
  FORCE_TRAILING = True

  def as_reply_params(self, user):
    num_invisible = sum(
        user.is_invisible for user in user.server.users.values())

    return ["There are {} users and {} invisible on {} servers".format(
        len(user.server.users) - num_invisible,
        num_invisible,
        1)]


class NameReply(message.Reply):
  NAME = "353"
  FORCE_TRAILING = True

  def __init__(self, type, channel_name, users):
    self.type = type
    self.channel_name = channel_name
    self.users = users

  def as_reply_params(self, user):
    return [self.type,
            self.channel_name if self.channel_name is not None else "*",
            " ".join(target.sigil + target.user.nickname for target in self.users)]


class EndOfNames(message.Reply):
  NAME = "366"
  FORCE_TRAILING = True

  def __init__(self, channel_name=None):
    self.channel_name = channel_name

  def as_reply_params(self, user):
    return [self.channel_name if self.channel_name is not None else "*",
            "End of /NAMES list"]


@NamesFeature.register_command
class Names(message.Command):
  NAME = "NAMES"
  MIN_ARITY = 0

  def __init__(self, channel_names=None, *args):
    self.channel_names = channel_names.split(",") if channel_names is not None \
                                                  else None

  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    if self.channel_names is None:
      seen_nicknames = set()

      for chan in user.server.channels.values():
        if user.is_in_channel(chan):
          seen_nicknames.update(cu.user.normalized_nickname
                                for cu in chan.get_visible_users_for(user))
          user.send_reply(NameReply(
                "@", chan.name, chan.get_visible_users_for(user)))
          continue

        if chan.is_secret:
          continue

        channel_users = []

        for cu in chan.users.values():
          if not cu.user.is_invisible:
            seen_nicknames.add(cu.user.normalized_nickname)
            channel_users.append(cu)

        if channel_users:
          user.send_reply(NameReply("=", chan.name, channel_users))

      visible_users = []

      for target in user.server.users.values():
        if target.is_invisible and target is not user:
          continue

        if target.normalized_nickname not in seen_nicknames:
          seen_nicknames.add(target.normalized_nickname)
          visible_users.append(channel.ChannelUser(None, target))

      if visible_users:
        user.send_reply(NameReply("*", None, visible_users))

      user.send_reply(EndOfNames(None))
    else:
      for channel_name in self.channel_names[:MAX_TARGETS]:
        try:
          chan = user.server.get_channel(channel_name)
        except errors.NoSuchNick:
          pass
        else:
          if not user.can_see_channel(chan):
            continue

          channel_name = chan.name

          user.send_reply(NameReply(
              "@" if user.is_in_channel(chan) else "*",
              chan.name,
              chan.get_visible_users_for(user)))
        user.send_reply(EndOfNames(channel_name))


@NamesFeature.hook("modify_targmax")
def modify_targmax(targmax):
  targmax["NAMES"] = MAX_TARGETS


@NamesFeature.hook("after_join_channel")
def send_names_on_join(user, target, channel):
    target.on_message(target.hostmask, Names(channel.name))


@NamesFeature.hook("luser_user")
def show_luser_oper(user):
  user.send_reply(LUserClient())


@NamesFeature.register_user_mode
class Invisible(mode.FlagMode, mode.UModeMixin):
  CHAR = "i"

  def toggle(self):
    self.target.is_invisible = not self.target.is_invisible
    return True

  def get(self):
    return self.target.is_invisible
