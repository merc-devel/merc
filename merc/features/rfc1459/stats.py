import datetime
import math

from merc import errors
from merc import feature
from merc import message
from merc import emitter


class StatsFeature(feature.Feature):
  NAME = __name__


install = StatsFeature.install


class StatsLinkInfo(message.Reply):
  NAME = "211"

  def __init__(self, linkname, sendq, sent_messages, sent_bytes, recv_messages, recv_bytes, open_time):
    self.linkname = linkname
    self.sendq = sendq
    self.sent_messages = sent_messages
    self.sent_bytes = sent_bytes
    self.recv_messages = recv_messages
    self.recv_bytes = recv_bytes
    self.open_time = open_time

  def as_reply_params(self):
    return [self.linkname, str(self.sendq), str(self.sent_messages),
            str(self.sent_bytes), str(self.recv_messages), str(self.recv_bytes),
            str(round(self.open_time.total_seconds()))]


class StatsCommands(message.Reply):
  NAME = "212"

  def __init__(self, command, count):
    self.command = command
    self.count = count

  def as_reply_params(self):
    return [self.command, str(self.count)]


class StatsUptime(message.Reply):
  NAME = "242"

  def __init__(self, uptime):
    total = uptime.total_seconds()
    self.days, total = divmod(total, 84600)
    self.hours, total = divmod(total, 3600)
    self.minutes, total = divmod(total, 60)

    self.days = math.floor(self.days)
    self.hours = math.floor(self.hours)
    self.minutes = math.floor(self.minutes)
    self.seconds = round(total)

  def as_reply_params(self):
    return ["Server Up {days} days {hours}:{minutes:02}:{seconds:02}".format(
        days=self.days, hours=self.hours, minutes=self.minutes, seconds=self.seconds)]


class EndOfStats(message.Reply):
  NAME = "219"

  def __init__(self, type):
    self.type = type

  def as_reply_params(self):
    return [self.type, "End of /STATS report"]


class StatsCLine(message.Reply):
  NAME = "213"

  def __init__(self, host, name, port, type):
   self.host = host
   self.name = name
   self.port = port
   self.type = type

  def as_reply_params(self):
    return ["C", self.host, "*", self.name, str(self.port), self.type]


class StatsHLine(message.Reply):
  NAME = "244"

  def __init__(self, hostmask, server):
    self.hostmask = hostmask
    self.server = server_name

  def as_reply_params(self):
    return ["H", self.hostmask, "*", self.server]



@StatsFeature.register_user_command
class Stats(message.Command):
  NAME = "STATS"
  MIN_ARITY = 1

  def __init__(self, type, server=None, *args):
    self.type = type
    self.server = server

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    if self.server is not None and self.server != app.server.name:
      raise errors.NoSuchServer(self.server_name)

    app.run_hooks("server.stats", user, self.type)
    app.run_hooks("server.stats." + self.type, user)
    user.send_reply(EndOfStats(self.type))


@StatsFeature.hook("server.stats.c")
def send_allowed_links(app, user):
  try:
    user.check_is_irc_operator()
  except errors.BaseError as e:
    user.send_reply(e)
  else:
    for name, link in app.config["links"].items():
      type = "hub" if link["hub"] else "server"
      user.send_reply(StatsCLine("*@{}".format(link["host"]), name, link["port"], type))

@StatsFeature.hook("server.stats.h")
def send_hubs(app, user):
  try:
    user.check_is_irc_operator()
  except errors.BaseError as e:
    user.send_reply(e)
  else:
    for name, link in app.config["links"].items():
      if not link["hub"]:
        continue
      user.send_reply(StatsHLine("*@{}".format(link["host"]), name))

@StatsFeature.hook("server.stats.l")
def send_links(app, user):
  try:
    user.check_is_irc_operator()
  except errors.BaseError as e:
    user.send_reply(e)
  else:
    # TODO: keep track of bytes sent/received?

    for user in app.users.all():
      if not user.is_local:
        continue
      mask = emitter.emit_hostmask(None, user.username, user.host)
      user.send_reply(StatsLinkInfo("{}[{}]".format(user.username, mask),
          0, 0, 0, 0, 0, datetime.datetime.now() - user.creation_time))

    for server in app.network.neighbors():
      mask = "*@{}".format(app.config["links"][server.name]["host"])
      user.send_reply(StatsLinkInfo("{}[{}]".format(server.name, mask),
          0, 0, 0, 0, 0, 0))

@StatsFeature.hook("server.stats.m")
def send_commands(app, user):
  for feature in app.features.all():
    for command in feature.USER_COMMANDS.values():
      user.send_reply(StatsCommands(command.NAME, 0))

@StatsFeature.hook("server.stats.u")
def send_uptime(app, user):
  user.send_reply(StatsUptime(datetime.datetime.now() - app.creation_time))

