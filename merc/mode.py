class Mode(object):
  TAKES_PARAM = True
  DEFAULT = None

  def __init__(self, target):
    self.target = target

  def set(self, client, value):
    return False

  def unset(self, client, value):
    return False

  def get(self):
    return self.DEFAULT

  @classmethod
  def read_from(cls, target):
    try:
      mode = target.modes[cls.CHAR]
    except KeyError:
      return self.DEFAULT
    else:
      return mode.get()


class FlagMode(Mode):
  TAKES_PARAM = False
  DEFAULT = False

  def __init__(self, target):
    super().__init__(target)
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

  def get(self):
    return self.value
