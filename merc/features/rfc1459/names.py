from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import mode


class NamesFeature(feature.Feature):
  NAME = __name__


install = NamesFeature.install


MAX_TARGETS = 1


class LUserClient(message.Reply):
  NAME = "251"
  FORCE_TRAILING = True

  def __init__(self, num_users, num_invisible, num_servers):
    self.num_users = num_users
    self.num_invisible = num_invisible
    self.num_servers = num_servers

  def as_reply_params(self):
    return ["There are {} users and {} invisible on {} servers".format(
        self.num_users - self.num_invisible,
        self.num_invisible, self.num_servers)]


class NameReply(message.Reply):
  NAME = "353"
  FORCE_TRAILING = True

  def __init__(self, type, channel_name, users, multi_prefix, uhnames):
    self.type = type
    self.channel_name = channel_name
    self.users = users
    self.multi_prefix = False
    self.uhnames = False

  def as_reply_params(self):
    return [self.type,
            self.channel_name if self.channel_name is not None else "*",
            " ".join((target.sigils if self.multi_prefix
                                    else target.sigil) +
                     (target.user.hostmask if self.uhnames
                                           else target.user.nickname)
                     for target in self.users)]


class EndOfNames(message.Reply):
  NAME = "366"
  FORCE_TRAILING = True

  def __init__(self, channel_name=None):
    self.channel_name = channel_name

  def as_reply_params(self):
    return [self.channel_name if self.channel_name is not None else "*",
            "End of /NAMES list"]


@NamesFeature.register_command
class Names(message.Command):
  NAME = "NAMES"
  MIN_ARITY = 0

  def __init__(self, channel_names=None, *args):
    self.channel_names = channel_names.split(",") if channel_names is not None \
                                                  else None

  def make_name_reply(self, app, user, type, channel_name, users):
    reply = NameReply(type, channel_name, users, False, False)
    app.run_hooks("modify_name_reply", user, reply)
    return reply

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    if self.channel_names is None:
      seen_nicknames = set()

      for chan in app.channels.all():
        if user.is_in_channel(chan):
          seen_nicknames.update(cu.user.normalized_nickname
                                for cu in chan.get_visible_users_for(user))
          user.send_reply(self.make_name_reply(
              app, user, "@", chan.name, chan.get_visible_users_for(user)))
          continue

        if chan.is_secret:
          continue

        channel_users = []

        for cu in chan.users.values():
          if not cu.user.is_invisible:
            seen_nicknames.add(cu.user.normalized_nickname)
            channel_users.append(cu)

        if channel_users:
          user.send_reply(self.make_name_reply(app, user, "=", chan.name,
                                               channel_users))

      visible_users = []

      for target in app.users.all():
        if target.is_invisible and target is not user:
          continue

        if target.normalized_nickname not in seen_nicknames:
          seen_nicknames.add(target.normalized_nickname)
          visible_users.append(channel.ChannelUser(None, target))

      if visible_users:
        user.send_reply(self.make_name_reply(app, user, "*", None,
                                             visible_users))

      user.send_reply(EndOfNames(None))
    else:
      for channel_name in self.channel_names[:MAX_TARGETS]:
        try:
          chan = app.channels.get(channel_name)
        except errors.NoSuchNick:
          pass
        else:
          if not user.can_see_channel(chan):
            continue

          channel_name = chan.name

          user.send_reply(self.make_name_reply(
              app, user,
              "@" if user.is_in_channel(chan) else "*",
              chan.name, chan.get_visible_users_for(user)))
        user.send_reply(EndOfNames(channel_name))


@NamesFeature.hook("modify_targmax")
def modify_targmax(app, targmax):
  targmax["NAMES"] = MAX_TARGETS


@NamesFeature.hook("after_join_channel")
def send_names_on_join(app, user, target, channel):
    target.on_message(app, target.hostmask, Names(channel.name))


@NamesFeature.hook("luser_user")
def show_luser_oper(app, user):
  num_invisible = sum(
      user.is_invisible for user in app.users.all())

  user.send_reply(LUserClient(app.users.count(), num_invisible, 1))


@NamesFeature.register_user_mode
class Invisible(mode.FlagMode, mode.UModeMixin):
  CHAR = "i"

  def toggle(self):
    self.target.is_invisible = not self.target.is_invisible
    return True

  def get(self):
    return self.target.is_invisible
