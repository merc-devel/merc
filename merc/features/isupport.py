from merc import feature
from merc import message


class ISupportFeature(feature.Feature):
  NAME = __name__


install = ISupportFeature


class ISupport(message.Reply):
  NAME = "005"
  FORCE_TRAILING = True

  def __init__(self, support_params):
    self.support_params = support_params

  def as_reply_params(self, user):
    return ["{}={}".format(k, v) for k, v in self.support_params.items()] + \
        ["are supported by this server"]


@ISupportFeature.hook("send_isupport")
def send_isupport(user):
  user.send_reply(ISupport(user.server.isupport))
