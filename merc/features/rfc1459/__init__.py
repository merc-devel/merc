__all__ = ["admin", "away", "ban", "info", "invite", "ison", "join", "key",
           "kick", "limit", "list", "lusers", "mode", "motd", "names", "nick",
           "oper", "ping", "privmsg", "rehash", "role", "topic", "userhost",
           "version", "welcome", "who", "whois"]

import importlib
import imp


def install(app):
  for name in __all__:
    imp.reload(importlib.import_module("." + name, __name__)).install(app)
