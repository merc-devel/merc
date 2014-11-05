from merc import capability
from merc import feature


class UserhostInNamesFeature(feature.Feature):
  NAME = __name__


install = UserhostInNamesFeature.install


@UserhostInNamesFeature.register_user_capability
class UserhostInNames(capability.Capability):
  NAME = "userhost-in-names"


@UserhostInNamesFeature.hook("server.names.modify")
def modify_name_reply(app, user, reply):
  if UserhostInNames(user).get():
    reply.uhnames = True
