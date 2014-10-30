from merc import capability
from merc import feature


class MultiPrefixFeature(feature.Feature):
  NAME = __name__


install = MultiPrefixFeature.install


@MultiPrefixFeature.register_user_capability
class MultiPrefix(capability.Capability):
  NAME = "multi-prefix"


@MultiPrefixFeature.hook("server.names.modify")
def modify_name_reply(app, user, reply):
  if MultiPrefix(user).get():
    reply.multi_prefix = True


@MultiPrefixFeature.hook("server.who.modify")
def modify_who_reply(app, user, target, reply):
  if MultiPrefix(user).get():
    reply.multi_prefix = True
