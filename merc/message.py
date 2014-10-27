import functools

from merc import emitter


class MessageTooLongError(Exception):
  pass


class Message(object):
  MAX_LENGTH = 510
  FORCE_TRAILING = False

  def emit(self, user, prefix):
    emitted = emitter.emit_message(prefix, self.NAME, self.as_params(user),
                                   force_trailing=self.FORCE_TRAILING)

    if len(emitted) > self.MAX_LENGTH:
      raise MessageTooLongError

    return emitted

  def as_params(self, user):
    raise NotImplementedError


class Reply(Message):
  def as_params(self, user):
    return [user.displayed_nickname] + self.as_reply_params()

  def as_reply_params(self):
    raise NotImplementedError


class Command(Message):
  def __init__(self, *args):
    pass

  def as_params(self, user):
    return self.as_command_params()

  def as_command_params(self):
    raise NotImplementedError

  @classmethod
  def with_params(cls, params):
    from merc import errors

    if len(params) < cls.MIN_ARITY:
      raise errors.NeedMoreParams(cls.NAME)

    return cls(*params)

  def handle_for(self, app, user, prefix):
    raise NotImplementedError

  @staticmethod
  def requires_registration(f):
    @functools.wraps(f)
    def _wrapper(self, app, user, prefix):
      from merc import errors

      if not user.is_registered:
        raise errors.NotRegistered

      f(self, app, user, prefix)
    return _wrapper

