import collections
import datetime


ListDetail = collections.namedtuple("ListDetail", ["server", "creation_time"])


class Mode(object):
  DEFAULT = None

  def __init__(self, target):
    self.target = target

  def set(self, server, user, value):
    raise NotImplementedError

  def unset(self, server, user, value):
    raise NotImplementedError

  def get(self):
    return self.target.modes.get(self.CHAR, self.DEFAULT)


class ChanModeMixin(object):
  def check(self, server, user, arg):
    self.target.check_is_operator(user)


class UModeMixin(object):
  def check(self, server, user, arg):
    if self.target is not user:
      raise errors.UsersDontMatch


class FlagMode(Mode):
  TAKES_PARAM = False
  DEFAULT = False

  def toggle(self):
    self.target.modes[self.CHAR] = not self.get()
    return True

  def set(self, server, user, value):
    if self.get():
      return False

    return self.toggle()

  def unset(self, server, user, value):
    if not self.get():
      return False

    return self.toggle()


class ListMode(Mode):
  TAKES_PARAM = True
  MAX_ITEMS = 100

  def check(self, server, user, arg):
    if arg is not None:
      super().check(server, user, arg)

  def add(self, server, user, value):
    list = self.target.modes.setdefault(self.CHAR, {})

    if value in list:
      return False

    if len(list) >= MAX_ITEMS:
      return False

    list[value] = ListDetail(server.name, datetime.datetime.now())
    return True

  def remove(self, server, user, value):
    list = self.target.modes.get(self.CHAR, {})
    if value not in list:
      return False

    del list[value]
    return True

  def list(self, user):
    raise NotImplementedError

  def set(self, server, user, value):
    if value is None:
      self.list(user)
      return False

    return self.add(server, user, value)

  def unset(self, server, user, value):
    if value is None:
      return False

    return self.remove(server, user, value)

  def get(self):
    return None


class SetWithParamMode(Mode):
  TAKES_PARAM = True

  def mutate(self, user, value):
    self.target.modes[self.CHAR] = value

  def set(self, server, user, value):
    if self.get() == value:
      return False

    return self.mutate(server, user, value)

  def unset(self, server, user, value):
    if value is None:
      return False

    if self.get() is None:
      return False

    return self.mutate(server, user, None)


class ParamMode(SetWithParamMode):
  TAKES_PARAM = True

  def unset(self, server, user, value):
    if value is None:
      return False

    if self.get() != value:
      return False

    return super().unset(server, user, value)


class ChannelRoleMode(Mode):
  TAKES_PARAM = True

  def toggle_for_target(self, target):
    raise NotImplementedError

  def get_for_target(self, target):
    raise NotImplementedError

  def check_for_target(self, user, target):
    self.target.check_is_operator(user)

  def check(self, server, user, value):
    target = self.target.get_channel_user_for(server.users.get(value))
    self.check_for_target(user, target)

  def set(self, server, user, value):
    target = self.target.get_channel_user_for(server.users.get(value))

    if self.get_for_target(target):
      return False

    self.toggle_for_target(target)
    return True

  def unset(self, server, user, value):
    target = self.target.get_channel_user_for(server.users.get(value))

    if not self.get_for_target(target):
      return False

    self.toggle_for_target(target)
    return True

  def get(self):
    return None
