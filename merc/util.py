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


ID_BASE = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"


def uidify(i):
  MAX_LENGTH = 6

  parts = []

  while i > 0:
    parts.append(ID_BASE[i % len(ID_BASE)])
    i //= len(ID_BASE)

  parts.reverse()

  if len(parts) > MAX_LENGTH:
    raise ValueError("number does not convert to a uid")

  return "".join(parts).rjust(MAX_LENGTH, "A")


def is_sid(sid):
  return len(sid) == 3 and sid[0].isdigit() and all(c in ID_BASE for c in sid)
