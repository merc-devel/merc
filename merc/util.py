import datetime
import subprocess

from merc import message


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


def is_uid(uid):
  if len(uid) != 9:
    return False

  return is_sid(uid[:3]) and all(c in ID_BASE for c in uid)


def parse_duration(s):
  years, _, s = s.rpartition("y")
  weeks, _, s = s.rpartition("w")
  days, _, s = s.rpartition("d")
  hours, _, s = s.rpartition("h")
  minutes, _, s = s.rpartition("m")
  seconds = s.rstrip("s")

  years = int(years) if years else 0
  weeks = int(weeks) if weeks else 0
  days = int(days) if days else 0
  hours = int(hours) if hours else 0
  minutes = int(minutes) if minutes else 0
  seconds = int(seconds) if seconds else 0

  return datetime.timedelta(
      seconds=seconds + (minutes +
          (hours + (days + weeks * 7 + years * 365) * 24) * 60) * 60)


def split_reply(make_reply, user, prefix, args):
  current_args = []

  for arg in args:
    current_args.append(arg)

    try:
      reply = make_reply(current_args)
      reply.emit(user, prefix)
    except message.MessageTooLongError:
      current_args.pop()
      yield make_reply(current_args)
      current_args = [arg]

  if current_args:
    yield make_reply(current_args)
