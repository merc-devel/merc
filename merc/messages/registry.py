from merc.messages import commands
from merc.messages import errors

REGISTRY = {}


def register(type):
  REGISTRY[type.NAME] = type

register(commands.Nick)
register(commands.User)
register(commands.Privmsg)
register(commands.Notice)
register(commands.Join)
