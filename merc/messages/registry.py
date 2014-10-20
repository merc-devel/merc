from merc.messages import commands
from merc.messages import errors

REGISTRY = {}


def register(type):
  REGISTRY[type.NAME] = type

register(commands.Nick)
register(commands.User)
register(commands.LUsers)
register(commands.Motd)
register(commands.Ping)
register(commands.Privmsg)
register(commands.Notice)
register(commands.Join)
register(commands.Part)
register(commands.Names)
register(commands.Topic)
