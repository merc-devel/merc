from merc import channel
from merc import feature
from merc import message


class ISupportFeature(feature.Feature):
  NAME = __name__


install = ISupportFeature.install


class ISupport(message.Reply):
  NAME = "005"
  FORCE_TRAILING = True

  def __init__(self, support_params):
    self.support_params = support_params

  def as_reply_params(self):
    return ["{}={}".format(k, v) for k, v in self.support_params.items()] + \
        ["are supported by this server"]


@ISupportFeature.hook("send_isupport")
def send_isupport(server, user):
  targmax = {}
  server.run_hooks("modify_targmax", targmax)

  isupport = {
      "NETWORK": server.network_name,
      "CASEMAPPING": "unicode",
      "CHARSET": "utf-8",
      "TARGMAX": ",".join("{}:{}".format(k, v if v is not None else "")
                          for k, v in targmax.items())
  }
  server.run_hooks("modify_isupport", isupport)

  reply = ISupport({})

  for k, v in isupport.items():
    reply.support_params[k] = v

    try:
      reply.emit(user, server.name)
    except message.MessageTooLongError:
      del reply.support_params[k]
      user.send_reply(reply)
      reply.support_params = {}

  if reply.support_params:
    user.send_reply(reply)
