import collections
import datetime


ListDetail = collections.namedtuple("ListDetail", ["server", "creation_time"])


class Mode(object):
  DEFAULT = None

  def __init__(self, target):
    self.target = target

  def set(self, user, value):
    raise NotImplementedError

  def unset(self, user, value):
    raise NotImplementedError

  def get(self):
    return self.target.modes.get(self.CHAR, self.DEFAULT)


class FlagMode(Mode):
  TAKES_PARAM = False
  DEFAULT = False

  def toggle(self):
    self.target.modes[self.CHAR] = not self.get()
    return True

  def set(self, user, value):
    if self.get():
      return False

    return self.toggle()

  def unset(self, user, value):
    if not self.get():
      return False

    return self.toggle()


class ListMode(Mode):
  TAKES_PARAM = True

  def add(self, user, value):
    list = self.target.modes.setdefault(self.CHAR, {})
    if value in list:
      return False

    list[value] = ListDetail(user.server.name, datetime.datetime.now())
    return True

  def remove(self, user, value):
    list = self.target.modes.get(self.CHAR, {})
    if value not in list:
      return False

    del list[value]
    return True

  def list(self, user):
    raise NotImplementedError

  def set(self, user, value):
    if value is None:
      self.list(user)
      return False

    return self.add(user, value)

  def unset(self, user, value):
    if value is None:
      return False

    return self.remove(user, value)

  def get(self):
    return None


class SetWithParamMode(Mode):
  TAKES_PARAM = True

  def mutate(self, value):
    self.target.modes[self.CHAR] = value

  def set(self, user, value):
    if self.get() == value:
      return False

    return self.mutate(value)

  def unset(self, user, value):
    if value is None:
      return False

    if self.get() is None:
      return False

    return self.mutate(None)


class ParamMode(SetWithParamMode):
  TAKES_PARAM = True

  def unset(self, user, value):
    if value is None:
      return False

    if self.get() != value:
      return False

    return super().unset(user, value)
