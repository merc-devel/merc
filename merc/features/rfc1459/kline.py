import collections
import datetime

from merc import errors
from merc import feature
from merc import message
from merc import util


class KLineFeature(feature.Feature):
  NAME = __name__


install = KLineFeature.install


KLineDetail = collections.namedtuple("KLineDetail", ["reason", "expiry"])


@KLineFeature.register_user_command
class KLine(message.Command):
  NAME = "KLINE"
  MIN_ARITY = 1

  def __init__(self, hostmask, duration=None, reason=None, *args):
    self.hostmask = hostmask
    self.duration = duration
    self.reason = reason

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    user.check_is_irc_operator()

    locals = app.server.get_feature_locals(KLineFeature)
    klines = locals.setdefault("klines", {})

    if self.hostmask[0] == "-":
      hostmask = self.hostmask[1:]
      try:
        del klines[hostmask]
      except KeyError:
        pass
      return

    if self.duration is not None:
      duration = util.parse_duration(self.duration)
      if duration == datetime.timedelta(seconds=0):
        expiry = None
      else:
        expiry = datetime.datetime.now() + duration
    else:
      expiry = None

    klines[self.hostmask] = KLineDetail(self.reason, expiry)

    for target in app.users.all():
      if target.hostmask_matches(self.hostmask):
        target.send(None, errors.LinkError("K-Lined"))
        target.close("K-Lined")


@KLineFeature.hook("user.register.check")
def check_klines(app, user):
  locals = app.server.get_feature_locals(KLineFeature)
  klines = locals.get("klines", {})

  now = datetime.datetime.now()

  for hostmask, detail in list(klines.items()):
    if user.hostmask_matches(hostmask):
      if detail.expiry is not None and detail.expiry < now:
        del klines[hostmask]
        continue

      user.send(None, errors.LinkError("K-Lined"))
      user.close("K-Lined")
