__all__ = ["chghost", "userhost_in_names"]


import importlib
import imp


def install(app):
  for name in __all__:
    imp.reload(importlib.import_module("." + name, __name__)).install(app)
