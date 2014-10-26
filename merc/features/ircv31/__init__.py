__all__ = ["cap", "multi_prefix", "uhnames"]


import importlib
import imp


def install(server):
  for name in __all__:
    imp.reload(importlib.import_module("." + name, __name__)).install(server)
