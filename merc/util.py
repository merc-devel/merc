import operator
import subprocess


def to_irc_lower(s):
  return s.casefold()


def make_flag_pair(mutator):
  return (lambda o, client, param=None: mutator(o, client, True),
          lambda o, client, param=None: mutator(o, client, False))


def immutable_mutator(o, client, param=None):
  return False


def make_immutable_flag_pair():
  return (immutable_mutator, immutable_mutator)


def is_channel_name(name):
  return name and name[0] == "#"


def show_modes(modes):
  flags = []
  args = []

  for k, v in sorted(modes.items(), key=operator.itemgetter(0)):
    flags.append(k)

    if v is not True:
      args.append(v)

  return "+" + "".join(flags), args


def get_version():
  try:
    hash = subprocess.check_output([
        "git", "rev-parse", "--short", "HEAD"]).strip().decode("utf-8")
    is_dirty = bool(subprocess.check_output(["git", "status", "--porcelain"]))
  except subprocess.CalledProcessError:
    return "HEAD"
  else:
    return hash + ("-dirty" if is_dirty else "")
