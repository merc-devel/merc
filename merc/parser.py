class ParseError(Exception):
  pass

def parse_hostmask(s):
  nickname, _, s = s.partition("!")
  username, _, host = s.partition("@")

  if not nickname or not username or not host:
    raise ParseError("malformed hostmask")

  return nickname, username, host


def parse_message(s):
  s = s.decode("utf-8")

  if not s:
    raise ParseError("malformed message")

  prefix = None

  if s[0] == ":":
    prefix, _, s = s[1:].partition(" ")

  s, trailing_separator, trailing = s.partition(" :")

  command, _, raw_params = s.partition(" ")
  command = command.upper()

  if not command:
    raise ParseError("malformed message")

  params = raw_params.split(" ") if raw_params else []

  if trailing_separator:
    params.append(trailing)

  return prefix, command, params
