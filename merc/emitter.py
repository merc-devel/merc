class EmitterError(Exception):
  pass


def emit_hostmask(nickname, username, host):
  if nickname is None:
    return "{}@{}".format(username, host)
  return "{}!{}@{}".format(nickname, username, host)


def emit_message(prefix, command, params, force_trailing=False):
  buf = []

  if prefix is not None:
    buf.append(":" + prefix)

  buf.append(command)

  if params:
    *init, last = params

    for param in init:
      if " " in param:
        raise EmitterError("malformed message")

    if force_trailing or not last or " " in last or last[0] == ":":
      buf.extend(init)
      buf.append(":" + last)
    else:
      buf.extend(params)

  return " ".join(buf).encode("utf-8")
