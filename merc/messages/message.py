from merc import emitter


class Message(object):
  def as_params(self, client):
    raise NotImplementedError

  def emit(self, client, prefix):
    return emitter.emit_message(prefix, self.NAME, self.as_params(client))
