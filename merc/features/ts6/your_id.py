from merc import feature
from merc import message


class YourIDFeature(feature.Feature):
  NAME = __name__


install = YourIDFeature.install


class YourID(message.Reply):
  NAME = "042"
  FORCE_TRAILING = True

  def __init__(self, uid):
    self.uid = uid

  def as_reply_params(self):
    return [self.uid, "your unique ID"]


@YourIDFeature.hook("after_welcome")
def send_your_id_on_welcome(app, user):
  user.send_reply(YourID(user.uid))
