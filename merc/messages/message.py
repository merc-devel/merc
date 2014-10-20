from merc import emitter


class Message(object):
  @classmethod
  def with_params(cls, params):
    from merc.messages import errors

    if len(params) < cls.MIN_ARITY:
      raise errors.NeedMoreParams(cls.NAME)

    return cls(*params)

  def as_params(self, client):
    raise NotImplementedError

  def emit(self, client, prefix):
    return emitter.emit_message(prefix, self.NAME, self.as_params(client))

  def handle_for(self, client, prefix):
    pass
