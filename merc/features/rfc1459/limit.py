from merc import channel
from merc import errors
from merc import feature
from merc import mode


class LimitFeature(feature.Feature):
  NAME = __name__


install = LimitFeature.install


class ChannelIsFull(errors.ParametrizedError):
  NAME = "471"
  REASON = "Channel is full"


@LimitFeature.register_channel_mode
class Limit(mode.SetWithParamMode, mode.ChanModeMixin):
  CHAR = "l"

  def mutate(self, user, value):
    locals = self.target.get_feature_locals(LimitFeature)
    limit = locals.get("limit", None)

    if value is not None:
      try:
        new_limit = int(value)
      except ValueError:
        return False
    else:
      new_limit = None

    if limit == new_limit:
      return False

    locals["limit"] = new_limit
    return True

  def get(self):
    locals = self.target.get_feature_locals(LimitFeature)
    return locals.get("limit", None)


@LimitFeature.hook("check_join_channel")
def check_channel_limit(target, channel, key):
  locals = channel.get_feature_locals(LimitFeature)
  limit = locals.get("limit", None)

  if limit is not None and len(channel.users) > limit - 1:
    raise ChannelIsFull(channel.name)
