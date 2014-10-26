import collections
import datetime

from merc import errors
from merc import feature
from merc import message


class FloodFeature(feature.Feature):
  NAME = __name__


install = FloodFeature.install


DETECT_LINES = 5
DETECT_TIME = datetime.timedelta(seconds=5)


@FloodFeature.hook("after_user_privmsg")
@FloodFeature.hook("after_channel_privmsg")
def check_privmsg_flood(user, target):
  locals = user.get_feature_locals(FloodFeature)
  message_times = locals.setdefault("message_times",
                                    collections.deque(maxlen=DETECT_LINES))

  message_times.append(datetime.datetime.now())

  if len(message_times) >= DETECT_LINES:
    if message_times[-1] - message_times[0] <= DETECT_TIME:
      user.close("Excess flood")
