def to_irc_lower(s):
  return s.casefold()


def make_flag_pair(mutator):
  return (lambda o, param=None: mutator(o, True),
          lambda o, param=None: mutator(o, False))


def is_channel_name(name):
  return name and name[0] == "#"
