__all__ = ["cap", "multi_prefix"]


import importlib
import imp


def install(app):
  for name in __all__:
    imp.reload(importlib.import_module("." + name, __name__)).install(app)
