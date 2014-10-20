import merc
from merc.messages import message


class ReplyMessage(message.Message):
  def as_params(self, client):
    return [client.displayed_nickname] + self.as_reply_params(client)

  def as_reply_params(self, client):
    return []


class Welcome(ReplyMessage):
  NAME = "001"

  def as_reply_params(self, client):
    return ["Welcome to the {} Internet Relay Chat Network, {}".format(
        client.server.network_name,
        client.displayed_nickname)]


class YourHost(ReplyMessage):
  NAME = "002"

  def as_reply_params(self, client):
    return ["Your host is {}, running {}-{}".format(
        client.server.name,
        merc.__name__,
        merc.__version__)]


class Created(ReplyMessage):
  NAME = "003"

  def as_reply_params(self, client):
    return ["This server was created {}".format(
        client.server.creation_time.isoformat())]


class MyInfo(ReplyMessage):
  NAME = "004"

  def as_reply_params(self, client):
    from merc import client as c
    from merc import channel

    return [client.server.name,
            "{}-{}".format(merc.__name__, merc.__version__),
            "".join(list(sorted(c.Client.MODES))),
            "".join(list(sorted(channel.Channel.MODES_WITHOUT_PARAMS))),
            "".join(list(sorted(channel.Channel.MODES_WITH_PARAMS)))]


class ISupport(ReplyMessage):
  NAME = "005"

  def __init__(self, support_params):
    self.support_params = support_params

  def as_reply_params(self, client):
    return ["{}={}".format(k, v) for k, v in self.support_params.items()] + \
        ["are supported by this server"]


class LUserClient(ReplyMessage):
  NAME = "251"

  def as_reply_params(self, client):
    return ["There are {} users and {} invisible on {} servers".format(
        len(client.server.clients),
        0,
        1)]


class LUserOp(ReplyMessage):
  NAME = "252"

  def as_reply_params(self, client):
    return ["0", "IRC operators online"]

class LUserUnknown(ReplyMessage):
  NAME = "253"

  def as_reply_params(self, client):
    return ["0", "unknown connections"]

class LUserChannels(ReplyMessage):
  NAME = "254"

  def as_reply_params(self, client):
    return [str(len(client.server.channels)), "channels formed"]

class LUserMe(ReplyMessage):
  NAME = "255"

  def as_reply_params(self, client):
    return ["I have {} clients and {} servers".format(
        len(client.server.clients),
        1)]


class Motd(ReplyMessage):
  NAME = "372"

  def __init__(self, line):
    self.line = line

  def as_reply_params(self, client):
    return ["- {}".format(self.line)]


class MotdStart(ReplyMessage):
  NAME = "375"

  def as_reply_params(self, client):
    return ["- {} Message of the Day".format(client.server.name)]


class EndOfMotd(ReplyMessage):
  NAME = "376"

  def as_reply_params(self, client):
    return ["End of /MOTD command"]


class NameReply(ReplyMessage):
  NAME = "353"

  def __init__(self, _, channel_name, users):
    self.channel_name = channel_name
    self.users = users

  def as_reply_params(self, client):
    return ["@", self.channel_name,
            " ".join(user.client.nickname for user in self.users.values())]


class EndOfNames(ReplyMessage):
  NAME = "366"

  def __init__(self, channel_name):
    self.channel_name = channel_name

  def as_reply_params(self, client):
    return [self.channel_name, "End of /NAMES list"]
