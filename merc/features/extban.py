from merc import errors
from merc import feature


class ExtBanFeature(feature.Feature):
  NAME = __name__


install = ExtBanFeature


def join_ban_checker(user, channel, value):
  try:
    origin_channel = user.server.get_channel(value)
  except errors.NoSuchNick:
    return

  if user.is_in_channel(origin_channel):
    raise errors.BannedFromChannel(channel.name)


CHECKERS = {
    "j": join_ban_checker
}


def check_ban(user, channel, mask):
  mode, colon, value = mask.partition(":")

  if colon != ":" or len(mode) != 1:
    return

  checker = CHECKERS.get(mode, None)

  if checker is not None:
    checker(user, channel, value)


@ExtBanFeature.hook("check_join_ban_mask")
def check_join_ban_mask(user, channel, mask):
  check_ban(user, channel, mask)


@ExtBanFeature.hook("check_message_ban_mask")
def check_message_ban_mask(user, channel, mask):
  try:
    check_ban(user, channel, mask)
  except errors.BannedFromChannel:
    channel.check_is_voiced(user)
