from merc import feature
from merc import message
from merc import util


class ISupportFeature(feature.Feature):
  NAME = __name__


install = ISupportFeature.install


@ISupportFeature.register_server_command
class ISupport(message.Reply):
  NAME = "005"
  FORCE_TRAILING = True
  MIN_ARITY = 1

  def __init__(self, *args):
    self.args = list(args)

  def as_reply_params(self):
    return self.args


def make_isupport_reply(params):
  return ISupport(*(
    ["{}={}".format(k, v) for k, v in params] + \
    ["are supported by this server"]))


@ISupportFeature.hook("server.isupport.send")
def send_isupport(app, user):
  targmax = {}
  app.run_hooks("server.targmax.modify", targmax)

  isupport = {
      "NETWORK": app.network_name,
      "CASEMAPPING": "unicode",
      "CHARSET": "utf-8",
      "TARGMAX": ",".join("{}:{}".format(k, v if v is not None else "")
                          for k, v in targmax.items())
  }
  app.run_hooks("server.isupport.modify", isupport)

  for reply in util.split_reply(make_isupport_reply, user, app.server.name,
                                list(isupport.items())):
    user.send_reply(reply)
