import collections
from datetime import datetime, date

from merc import util
from merc import feature
from merc import message

INFO_TEMPLATE="""
   _ __ ___   ___ _ __ ___
  | '_ ` _ \ / _ \ '__/ __|
  | | | | | |  __/ | | (__ _
  |_| |_| |_|\___|_|  \___(_)

  The Modern Ethereal Relay Chat daemon, version {version}.
  Copyright (C) {year}, #merc-devel

  This software comes with no warranties: see the LICENSE file in the
  source root for details and usage terms.

  Visit us: http://merc-devel.com
  Get the merc source code at: https://github.com/merc-devel/merc

  The following people have contributed significantly to merc,
  in nickname-alphabetic order:
  - rfw <press@rfw.name>
  - Shiz <hi@shiz.me>

  This merc instance has been online since {online_since},
  meaning it has been up for {online_for}!
"""


class InfoFeature(feature.Feature):
  NAME = __name__


install = InfoFeature.install


class InfoReply(message.Reply):
  NAME = "371"
  FORCE_TRAILING = True

  def __init__(self, line):
    self.line = line

  def as_reply_params(self, user):
    return [self.line]

class EndOfInfo(message.Reply):
  NAME = "374"

  def as_reply_params(self, user):
    return ["End of /INFO list"]


@InfoFeature.register_command
class Info(message.Command):
  NAME = "INFO"
  MIN_ARITY = 0

  @message.Command.requires_registration
  def handle_for(self, user, prefix):
    version = util.get_version()
    year = date.today().year
    online_since = user.server.creation_time.strftime("%c")
    online_for = friendly_timespan(datetime.now() - user.server.creation_time)

    lines = INFO_TEMPLATE.format(
        version=version,
        year=year,
        online_since=online_since,
        online_for=online_for)

    for line in lines.splitlines():
      user.send_reply(InfoReply(line))
    user.server.run_hooks("after_info", user)
    user.send_reply(EndOfInfo())

def friendly_timespan(diff, range=3):
  UNITS = collections.OrderedDict([
    ('year',   31536000),
    ('month',  2592000),
    ('week',   604800),
    ('day',    86400),
    ('hour',   3600),
    ('minute', 60),
    ('second', 1)
  ])
  seconds = round(diff.total_seconds())
  indications = []

  for unit, amount in UNITS.items():
    n, seconds = divmod(seconds, amount)
    if n == 0:
      continue
    elif n > 1:
      unit += "s"

    indications.append('{} {}'.format(n, unit))
    if range is not None:
      range -= 1
      if range == 0:
        break

  if len(indications) > 0:
    if len(indications) > 1:
      return ", ".join(indications[:-1]) + " and " + indications[-1]
    return indications[0]
  else:
    return "a small while"
