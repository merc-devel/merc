from merc import capability
from merc import feature


class UHNamesFeature(feature.Feature):
  NAME = __name__


install = UHNamesFeature.install


@UHNamesFeature.register_capability
class UHNames(capability.Capability):
  NAME = "uhnames"


@UHNamesFeature.hook("modify_name_reply")
def modify_name_reply(server, user, reply):
  if UHNames(user).get():
    reply.uhnames = True
