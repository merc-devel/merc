__all__ = ["admin", "away", "ban", "ison", "join", "key", "kick", "limit",
           "list", "lusers", "mode", "motd", "names", "nick", "oper", "ping",
           "privmsg", "rehash", "role", "topic", "userhost", "version",
           "welcome", "who", "whois"]


import importlib
import imp


def install(server):
  for name in __all__:
    imp.reload(importlib.import_module("." + name, __name__)).install(server)
