import operator


def to_irc_lower(s):
  return s.casefold()


def make_flag_pair(mutator):
  return (lambda o, param=None: mutator(o, True),
          lambda o, param=None: mutator(o, False))


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
