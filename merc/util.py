import operator
import subprocess


def to_irc_lower(s):
  return s.casefold()


def get_version():
  try:
    hash = subprocess.check_output([
        "git", "rev-parse", "--short", "HEAD"]).strip().decode("utf-8")
    is_dirty = bool(subprocess.check_output(["git", "status", "--porcelain"]))
  except subprocess.CalledProcessError:
    return "HEAD"
  else:
    return hash + ("-dirty" if is_dirty else "")


class Expando(object):
  def __init__(self, **kwargs):
    self.__dict__ = kwargs
