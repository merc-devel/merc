from merc import errors
from merc import feature
from merc import message


class LinksFeature(feature.Feature):
  NAME = __name__


install = LinksFeature.install


class LinksReply(message.Reply):
  NAME = "364"
  FORCE_TRAILING = True
  MIN_ARITY = 3

  def __init__(self, type, server_name, info, *args):
    self.type = type
    self.server_name = server_name
    self.info = info

  def as_reply_params(self):
    return [self.type, self.server_name, self.info]


class EndOfLinks(message.Reply):
  NAME = "365"
  FORCE_TRAILING = True
  MIN_ARITY = 2

  def __init__(self, mask, reason="End of /LINKS list", *args):
    self.mask = mask
    self.reason = reason

  def as_reply_params(self):
    return [self.mask, self.reason]


@LinksFeature.register_user_command
class Links(message.Command):
  NAME = "LINKS"
  MIN_ARITY = 0

  def __init__(self, server_name=None, *args):
    self.server_name = server_name

  @message.Command.requires_registration
  def handle_for(self, app, user, prefix):
    user.check_is_irc_operator()

    server_name = self.server_name if self.server_name is not None \
                                   else app.network.local.name

    try:
      source = app.network.get(server_name)
    except KeyError:
      raise errors.NoSuchServer(server_name)

    for origin, target in app.network.links(source):
      user.send(None,
                LinksReply(server_name, target.name,
                           "{} {}".format(abs(target.hopcount -
                                              source.hopcount),
                                          target.description)))
    user.send(None, EndOfLinks(server_name))
