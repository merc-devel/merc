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


def uidify(i):
  BASE = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  MAX_LENGTH = 6

  parts = []

  while i > 0:
    parts.append(BASE[i % len(BASE)])
    i //= len(BASE)

  parts.reverse()

  if len(parts) > MAX_LENGTH:
    raise ValueError("number does not convert to a uid")

  return "".join(parts).rjust(MAX_LENGTH, "A")
