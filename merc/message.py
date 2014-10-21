import functools

from merc import emitter


class Message(object):
  MAX_LENGTH = 510
  FORCE_TRAILING = False

  def as_params(self, client):
    raise NotImplementedError

  def emit(self, client, prefix):
    return emitter.emit_message(prefix, self.NAME, self.as_params(client),
                                force_trailing=self.FORCE_TRAILING)


class Reply(Message):
  def as_params(self, client):
    return [client.displayed_nickname] + self.as_reply_params(client)

  def as_reply_params(self, client):
    return []


class Command(Message):
  REGISTRY = {}

  @classmethod
  def register(cls, type):
    cls.REGISTRY[type.NAME] = type
    return type

  def __init__(self, *args):
    pass

  @classmethod
  def with_params(cls, params):
    from merc import errors

    if len(params) < cls.MIN_ARITY:
      raise errors.NeedMoreParams(cls.NAME)

    return cls(*params)

  def handle_for(self, client, prefix):
    pass

  @staticmethod
  def requires_registration(f):
    @functools.wraps(f)
    def _wrapper(self, client, prefix):
      from merc import errors

      if not client.is_registered:
        raise errors.NotRegistered

      f(self, client, prefix)
    return _wrapper

