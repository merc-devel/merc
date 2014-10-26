class Capability(object):
  def __init__(self, user):
    self.user = user

  def get(self):
    return self.NAME in self.user.capabilities

  def set(self):
    self.user.capabilities.add(self.NAME)

  def unset(self):
    self.user.capabilities.remove(self.NAME)
