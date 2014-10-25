import collections
import datetime

from merc import channel
from merc import errors
from merc import feature
from merc import message
from merc import mode


class BanFeature(feature.Feature):
  NAME = __name__


install = BanFeature


class BanList(message.Reply):
  NAME = "367"

  def __init__(self, channel_name, mask, server, creation_time):
    self.channel_name = channel_name
    self.mask = mask
    self.server = server
    self.creation_time = creation_time

  def as_reply_params(self, client):
    return [self.channel_name, self.mask, self.server,
            str(int(self.creation_time.timestamp()))]


class EndOfBanList(message.Reply):
  NAME = "368"
  FORCE_TRAILING = True

  def __init__(self, channel_name):
    self.channel_name = channel_name

  def as_reply_params(self, client):
    return [self.channel_name, "End of channel ban list"]


@BanFeature.register_channel_mode
class BanMask(mode.Mode):
  CHAR = "b"
  TAKES_PARAM = True

  def set(self, client, value):
    if value is None:
      # display all bans to the client instead
      for mask, detail in sorted(self.target.bans.items(),
                                 key=lambda v: v[1].creation_time,
                                 reverse=True):
        client.send_reply(BanList(self.target.name, mask, detail.server,
                                  detail.creation_time))
      client.send_reply(EndOfBanList(self.target.name))
      return False
    else:
      if value in self.target.bans:
        return False
      self.target.bans[value] = channel.BanDetail(client.server.name,
                                                  datetime.datetime.now())
    return True

  def unset(self, client, value):
    if value is None:
      return False

    if value not in self.target.bans:
      return False

    del self.target.bans[value]
    return True



def check_ban(user, channel):
  if any(user.hostmask_matches(mask) for mask in channel.bans):
    raise errors.BannedFromChannel(channel.name)


@BanFeature.hook("check_join_channel")
def check_channel_ban(user, channel, key):
  check_ban(user, channel)


@BanFeature.hook("check_can_message_channel")
def check_can_message_channel(user, channel):
  try:
    check_ban(user, channel)
  except errors.BannedFromChannel:
    channel.check_is_voiced(user)
