class Mode(object):
  TAKES_PARAM = True
  HIDDEN = False

  def set(self, client, value):
    raise NotImplementedError

  def unset(self, client, value):
    raise NotImplementedError

  def get_value(self):
    raise NotImplementedError


class FlagMode(Mode):
  TAKES_PARAM = False
  DEFAULT = False

  def __init__(self):
    self.value = self.DEFAULT

  def set(self, client, value):
    if self.value:
      return False

    self.value = True
    return True

  def unset(self, client, value):
    if not self.value:
      return False

    self.value = False
    return True

  def get_value(self):
    return self.value
