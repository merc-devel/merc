import functools

from merc import emitter


class MessageTooLongError(Exception):
  pass


class Message(object):
  MAX_LENGTH = 510
  FORCE_TRAILING = False

  def emit(self, client, prefix):
    emitted = emitter.emit_message(prefix, self.NAME, self.as_params(client),
                                   force_trailing=self.FORCE_TRAILING)

    if len(emitted) > self.MAX_LENGTH:
      raise MessageTooLongError

    return emitted

  def as_params(self, client):
    raise NotImplementedError

  def can_send_to(self, client):
    return True

  @classmethod
  def with_params(cls, params):
    from merc import errors

    if len(params) < cls.MIN_ARITY:
      raise errors.NeedMoreParams(cls.NAME)

    return cls(*params)


class Reply(Message):
  def as_params(self, client):
    return [client.displayed_nickname] + self.as_reply_params()

  def as_reply_params(self):
    raise NotImplementedError

  @classmethod
  def with_params(cls, params):
    target, *params = params
    reply = cls(*params)
    reply.client_target = target
    return reply


class Command(Message):
  def __init__(self, *args):
    pass

  def as_params(self, client):
    return self.as_command_params()

  def as_command_params(self):
    raise NotImplementedError

  def handle_for(self, app, client, prefix):
    raise NotImplementedError

  @staticmethod
  def requires_registration(f):
    @functools.wraps(f)
    def _wrapper(self, app, client, prefix):
      from merc import errors

      if not client.is_registered:
        raise errors.NotRegistered

      f(self, app, client, prefix)
    return _wrapper

