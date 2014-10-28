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


class BanList(message.Reply):
  NAME = "367"

  def __init__(self, channel_name, mask, server_name, creation_time):
    self.channel_name = channel_name
    self.mask = mask
    self.server_name = server_name
    self.creation_time = creation_time

  def as_reply_params(self):
    return [self.channel_name, self.mask, self.server_name,
            str(int(self.creation_time.timestamp()))]


class EndOfBanList(message.Reply):
  NAME = "368"
  FORCE_TRAILING = True

  def __init__(self, channel_name):
    self.channel_name = channel_name

  def as_reply_params(self):
    return [self.channel_name, "End of channel ban list"]


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
                                detail.creation_time))
    user.send_reply(EndOfBanList(self.target.name))

  def add(self, app, user, value):
    locals = self.target.get_feature_locals(BanFeature)
    bans = locals.setdefault("bans", {})

    if value in bans:
      return False

    bans[value] = BanDetail(app.server_name, datetime.datetime.now())
    return True

  def remove(self, user, value):
    locals = self.target.get_feature_locals(BanFeature)
    bans = locals.get("bans", {})

    if value not in bans:
      return False

    del bans[value]
    return True


@BanFeature.hook("check_join_channel")
def check_channel_ban(app, target, channel, key):
  locals = channel.get_feature_locals(BanFeature)

  for mask in locals.get("bans", {}):
    if target.hostmask_matches(mask):
      raise errors.BannedFromChannel(channel.name)

    app.run_hooks("check_join_ban_mask", target, channel, mask)


@BanFeature.hook("check_can_message_channel")
def check_can_message_channel(app, target, channel):
  locals = channel.get_feature_locals(BanFeature)

  for mask in locals.get("bans", {}):
    if target.hostmask_matches(mask):
      channel.check_is_voiced(target)

    app.run_hooks("check_message_ban_mask", target, channel, mask)
