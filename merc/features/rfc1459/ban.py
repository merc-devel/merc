import collections
import datetime

from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import mode


BanDetail = collections.namedtuple("BanDetail", ["app", "creation_time"])


class BanFeature(feature.Feature):
  NAME = __name__


install = BanFeature.install


@BanFeature.register_server_command
class BanList(message.Reply):
  NAME = "367"
  MIN_ARITY = 4

  def __init__(self, channel_name, mask, server_name, creation_time, *args):
    self.channel_name = channel_name
    self.mask = mask
    self.server_name = server_name
    self.creation_time = creation_time

  def as_reply_params(self):
    return [self.channel_name, self.mask, self.server_name,
            self.creation_time]


@BanFeature.register_server_command
class EndOfBanList(message.Reply):
  NAME = "368"
  FORCE_TRAILING = True
  MIN_ARITY = 2

  def __init__(self, channel_name, reason="End of channel ban list", *args):
    self.channel_name = channel_name
    self.reason = reason

  def as_reply_params(self):
    return [self.channel_name, self.reason]


@BanFeature.register_channel_mode
class BanMask(mode.ListMode, mode.ChanModeMixin):
  CHAR = "b"

  def list(self, user):
    locals = self.target.get_feature_locals(BanFeature)
    bans = locals.get("bans", {})

    for mask, detail in sorted(bans.items(),
                               key=lambda v: v[1].creation_time,
                               reverse=True):
      user.send_reply(BanList(self.target.name, mask, detail.app,
                               str(int(detail.creation_time.timestamp()))))
    user.send_reply(EndOfBanList(self.target.name))

  def add(self, app, user, value):
    locals = self.target.get_feature_locals(BanFeature)
    bans = locals.setdefault("bans", {})

    if value in bans:
      return False

    bans[value] = BanDetail(app.server.name, datetime.datetime.now())
    return True

  def remove(self, user, value):
    locals = self.target.get_feature_locals(BanFeature)
    bans = locals.get("bans", {})

    if value not in bans:
      return False

    del bans[value]
    return True


@BanFeature.hook("channel.join.check")
def check_channel_ban(app, target, channel, key):
  locals = channel.get_feature_locals(BanFeature)

  for mask in locals.get("bans", {}):
    if target.hostmask_matches(mask):
      raise errors.BannedFromChannel(channel.name)

    app.run_hooks("channel.join.check_ban", target, channel, mask)


@BanFeature.hook("channel.message.check")
def check_can_message_channel(app, target, channel):
  locals = channel.get_feature_locals(BanFeature)

  for mask in locals.get("bans", {}):
    if target.hostmask_matches(mask):
      channel.check_is_voiced(target)

    app.run_hooks("channel.message.check_ban", target, channel, mask)
