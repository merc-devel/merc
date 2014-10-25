import functools

from merc import emitter


class Message(object):
  MAX_LENGTH = 510
  FORCE_TRAILING = False

  def as_params(self, user):
    raise NotImplementedError

  def emit(self, user, prefix):
    return emitter.emit_message(prefix, self.NAME, self.as_params(user),
                                force_trailing=self.FORCE_TRAILING)


class Reply(Message):
  def as_params(self, user):
    return [user.displayed_nickname] + self.as_reply_params(user)

  def as_reply_params(self, user):
    return []


class Command(Message):
  def __init__(self, *args):
    pass

  @classmethod
  def with_params(cls, params):
    from merc import errors

    if len(params) < cls.MIN_ARITY:
      raise errors.NeedMoreParams(cls.NAME)

    return cls(*params)

  def handle_for(self, user, prefix):
    pass

  @staticmethod
  def requires_registration(f):
    @functools.wraps(f)
    def _wrapper(self, user, prefix):
      from merc import errors

      if not user.is_registered:
        raise errors.NotRegistered

      f(self, user, prefix)
    return _wrapper

