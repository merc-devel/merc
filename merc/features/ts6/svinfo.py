import datetime

from merc import errors
from merc import feature
from merc import message


class SvInfoFeature(feature.Feature):
  NAME = __name__


install = SvInfoFeature.install


@SvInfoFeature.register_server_command
class SvInfo(message.Command):
  NAME = "SVINFO"
  MIN_ARITY = 4

  def __init__(self, ts_version, min_ts_version, unused, current_time):
    self.ts_version = ts_version
    self.min_ts_version = min_ts_version
    self.unused = unused
    self.current_time = current_time

  def as_command_params(self):
    return [self.ts_version, self.min_ts_version, self.unused,
            self.current_time]

  def handle_for(self, app, server, prefix):
    try:
      ts_version = int(self.ts_version)
    except ValueError:
      raise errors.LinkError("Bad TS version")

    if ts_version < 8:
      raise errors.LinkError("Bad TS version")


@SvInfoFeature.hook("server.svinfo")
def send_svinfo(app, server):
  server.send(None, SvInfo("8", "8", "0",
                           str(int(datetime.datetime.now().timestamp()))))
