__all__ = ["burst", "connect", "pass", "quit", "sid", "svinfo", "uid",
           "your_id"]


import importlib
import imp


def install(app):
  for name in __all__:
    imp.reload(importlib.import_module("." + name, __name__)).install(app)
