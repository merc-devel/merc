from merc import emitter


class Message(object):
  MAX_LENGTH = 510
  FORCE_TRAILING = False

  def as_params(self, client):
    raise NotImplementedError

  def emit(self, client, prefix):
    return emitter.emit_message(prefix, self.NAME, self.as_params(client),
                                force_trailing=self.FORCE_TRAILING)
