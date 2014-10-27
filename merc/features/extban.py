from merc import errors
from merc import feature


class ExtBanFeature(feature.Feature):
  NAME = __name__


install = ExtBanFeature.install


def join_ban_checker(app, target, channel, value):
  try:
    origin_channel = app.channels.get(value)
  except errors.NoSuchNick:
    return

  if target.is_in_channel(origin_channel):
    raise errors.BannedFromChannel(channel.name)


CHECKERS = {
    "j": join_ban_checker
}


def check_ban(app, target, channel, mask):
  mode, colon, value = mask.partition(":")

  if colon != ":" or len(mode) != 1:
    return

  checker = CHECKERS.get(mode, None)

  if checker is not None:
    checker(app, target, channel, value)


@ExtBanFeature.hook("check_join_ban_mask")
def check_join_ban_mask(app, target, channel, mask):
  check_ban(app, target, channel, mask)


@ExtBanFeature.hook("check_message_ban_mask")
def check_message_ban_mask(app, target, channel, mask):
  try:
    check_ban(app, target, channel, mask)
  except errors.BannedFromChannel:
    channel.check_is_voiced(target)
