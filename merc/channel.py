class Channel(object):
  def __init__(self, name):
    self.name = name

    self.clients = set()

  def broadcast(self, prefix, message):
    for client in self.clients:
      client.send(prefix, message)

  __hash__ = id
