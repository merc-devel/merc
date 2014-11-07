import asyncio


class CountDownLatch(object):
  def __init__(self, count=0, *, loop=None):
    if loop is None:
      loop = asyncio.get_event_loop()

    self.count = count
    self.event = asyncio.Event(loop=loop)

  def increment(self, num=1):
    self.count += num

  def decrement(self, num=1):
    self.count -= num

    if self.count <= 0:
      self.count = 0
      self.event.set()

  def wait(self):
    yield from self.event.wait()
