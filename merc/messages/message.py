from merc import emitter


class Message(object):
  MAX_LENGTH = 510

  def as_params(self, client):
    raise NotImplementedError

  def emit(self, client, prefix):
    raw = emitter.emit_message(prefix, self.NAME, self.as_params(client))
    return raw.encode("utf-8")[:self.MAX_LENGTH].decode("utf-8", "ignore")
