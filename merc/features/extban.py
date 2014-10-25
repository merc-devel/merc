from merc import errors
from merc import feature


class ExtBanFeature(feature.Feature):
  NAME = __name__


install = ExtBanFeature


def join_ban_checker(target, channel, value):
  try:
    origin_channel = target.server.get_channel(value)
  except errors.NoSuchNick:
    return

  if target.is_in_channel(origin_channel):
    raise errors.BannedFromChannel(channel.name)


CHECKERS = {
    "j": join_ban_checker
}


def check_ban(target, channel, mask):
  mode, colon, value = mask.partition(":")

  if colon != ":" or len(mode) != 1:
    return

  checker = CHECKERS.get(mode, None)

  if checker is not None:
    checker(target, channel, value)


@ExtBanFeature.hook("check_join_ban_mask")
def check_join_ban_mask(target, channel, mask):
  check_ban(target, channel, mask)


@ExtBanFeature.hook("check_message_ban_mask")
def check_message_ban_mask(target, channel, mask):
  try:
    check_ban(target, channel, mask)
  except errors.BannedFromChannel:
    channel.check_is_voiced(target)
