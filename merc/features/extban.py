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


def check_ban(user, channel):
  for mask in channel.bans:
    mode, colon, value = mask.partition(":")

    if colon != ":" or len(mode) != 1:
      continue

    checker = CHECKERS.get(mode, None)

    if checker is not None:
      checker(user, channel, value)


@ExtBanFeature.hook("check_join_channel")
def check_channel_ban(user, channel, key):
  check_ban(user, channel)


@ExtBanFeature.hook("check_can_message_channel")
def check_can_message_channel(user, channel):
  try:
    check_ban(user, channel)
  except errors.BannedFromChannel:
    channel.check_is_voiced(user)
