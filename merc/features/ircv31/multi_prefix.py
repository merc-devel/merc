from merc import capability
from merc import feature


class MultiPrefixFeature(feature.Feature):
  NAME = __name__


install = MultiPrefixFeature.install


@MultiPrefixFeature.register_capability
class MultiPrefix(capability.Capability):
  NAME = "multi-prefix"


@MultiPrefixFeature.hook("modify_name_reply")
def modify_name_reply(app, user, reply):
  if MultiPrefix(user).get():
    reply.multi_prefix = True


@MultiPrefixFeature.hook("modify_who_reply")
def modify_who_reply(app, user, target, reply):
  if MultiPrefix(user).get():
    reply.multi_prefix = True
