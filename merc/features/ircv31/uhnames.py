from merc import capability
from merc import feature


class UHNamesFeature(feature.Feature):
  NAME = __name__


install = UHNamesFeature.install


@UHNamesFeature.register_user_capability
class UHNames(capability.Capability):
  NAME = "uhnames"


@UHNamesFeature.hook("server.names.modify")
def modify_name_reply(app, user, reply):
  if UHNames(user).get():
    reply.uhnames = True
