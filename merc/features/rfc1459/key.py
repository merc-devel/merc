from merc import channel
from merc import errors
from merc import feature
from merc import mode


class KeyFeature(feature.Feature):
  NAME = __name__


install = KeyFeature.install


class KeySet(errors.ParametrizedError):
  NAME = "467"
  REASON = "Key is already set for this channel"


class BadChannelKey(errors.ParametrizedError):
  NAME = "475"
  REASON = "Incorrect channel key"


@KeyFeature.register_channel_mode
class Key(mode.ParamMode, mode.ChanModeMixin):
  CHAR = "k"

  def mutate(self, user, value):
    locals = self.target.get_feature_locals(KeyFeature)
    key = locals.get("key", None)

    if key is not None and value is not None:
      user.send_reply(KeySet(self.target.name))
      return False

    if value == key:
      return False

    locals["key"] = value
    return True


@KeyFeature.hook("check_join_channel")
def check_channel_key(target, channel, key):
  locals = channel.get_feature_locals(KeyFeature)

  if key != locals.get("key", key):
    raise BadChannelKey(channel.name)
