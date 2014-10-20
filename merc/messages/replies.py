import merc

from merc import util
from merc.messages import message


class ReplyMessage(message.Message):
  def as_params(self, client):
    return [client.displayed_nickname] + self.as_reply_params(client)

  def as_reply_params(self, client):
    return []


class Welcome(ReplyMessage):
  NAME = "001"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["Welcome to the {} Internet Relay Chat Network, {}".format(
        client.server.network_name,
        client.displayed_nickname)]


class YourHost(ReplyMessage):
  NAME = "002"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["Your host is {}, running {}-{}".format(
        client.server.name,
        merc.__name__,
        merc.__version__)]


class Created(ReplyMessage):
  NAME = "003"
  FORCE_TRAILING = True

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
  FORCE_TRAILING = True

  def __init__(self, support_params):
    self.support_params = support_params

  def as_reply_params(self, client):
    return ["{}={}".format(k, v) for k, v in self.support_params.items()] + \
        ["are supported by this server"]


class UmodeIs(ReplyMessage):
  NAME = "221"

  def __init__(self, modes):
    self.modes = modes

  def as_reply_params(self, client):
    flags, args = util.show_modes(self.modes)
    return [flags] + args


class LUserClient(ReplyMessage):
  NAME = "251"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    num_invisible = sum(
        client.is_invisible for client in client.server.clients.values())

    return ["There are {} users and {} invisible on {} servers".format(
        len(client.server.clients) - num_invisible,
        num_invisible,
        1)]


class LUserOp(ReplyMessage):
  NAME = "252"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["0", "IRC operators online"]

class LUserUnknown(ReplyMessage):
  NAME = "253"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["0", "unknown connections"]


class LUserChannels(ReplyMessage):
  NAME = "254"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return [str(len(client.server.channels)), "channels formed"]


class LUserMe(ReplyMessage):
  NAME = "255"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["I have {} clients and {} servers".format(
        len(client.server.clients),
        1)]


class UserHost(ReplyMessage):
  NAME = "302"
  FORCE_TRAILING = True

  def __init__(self, user_hosts):
    self.user_hosts = user_hosts

  def as_reply_params(self, client):
    return [" ".join(self.user_hosts)]


class IsOn(ReplyMessage):
  NAME = "303"
  FORCE_TRAILING = True

  def __init__(self, nicknames):
    self.nicknames = nicknames

  def as_reply_params(self, client):
    return [" ".join(self.nicknames)]


class EndOfWho(ReplyMessage):
  NAME = "315"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["End of /WHO command"]


class ChannelModeIs(ReplyMessage):
  NAME = "324"

  def __init__(self, channel_name, modes):
    self.channel_name = channel_name
    self.modes = modes

  def as_reply_params(self, client):
    flags, args = util.show_modes(self.modes)
    return [self.channel_name, flags] + args


class CreationTime(ReplyMessage):
  NAME = "329"

  def __init__(self, channel_name, time):
    self.channel_name = channel_name
    self.time = time

  def as_reply_params(self, client):
    return [self.channel_name, str(int(self.time.timestamp()))]


class NoTopic(ReplyMessage):
  NAME = "331"
  FORCE_TRAILING = True

  def __init__(self, channel_name):
    self.channel_name = channel_name

  def as_reply_params(self, client):
    return [self.channel_name, "No topic set"]


class Topic(ReplyMessage):
  NAME = "332"
  FORCE_TRAILING = True

  def __init__(self, channel_name, text):
    self.channel_name = channel_name
    self.text = text

  def as_reply_params(self, client):
    return [self.channel_name, self.text]


class TopicWhoTime(ReplyMessage):
  NAME = "333"

  def __init__(self, channel_name, who, time):
    self.channel_name = channel_name
    self.who = who
    self.time = time

  def as_reply_params(self, client):
    return [self.channel_name, self.who, str(int(self.time.timestamp()))]


class Motd(ReplyMessage):
  NAME = "372"
  FORCE_TRAILING = True

  def __init__(self, line):
    self.line = line

  def as_reply_params(self, client):
    return ["- {}".format(self.line)]


class MotdStart(ReplyMessage):
  NAME = "375"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["- {} Message of the Day".format(client.server.name)]


class EndOfMotd(ReplyMessage):
  NAME = "376"
  FORCE_TRAILING = True

  def as_reply_params(self, client):
    return ["End of /MOTD command"]


class WhoReply(ReplyMessage):
  NAME = "352"
  FORCE_TRAILING = True

  def __init__(self, channel_name, user, hopcount, server):
    self.channel_name = channel_name
    self.user = user
    self.hopcount = hopcount
    self.server = server

  def as_reply_params(self, client):
    return [self.channel_name if self.channel_name is not None else "*",
            self.user.username, self.user.host, self.server, self.user.nickname,
            "H" if not self.user.is_away else "G",
            str(self.hopcount) + " " + self.user.realname]


class NameReply(ReplyMessage):
  NAME = "353"
  FORCE_TRAILING = True

  def __init__(self, type, channel_name, users):
    self.type = type
    self.channel_name = channel_name
    self.users = users

  def as_reply_params(self, client):
    return [self.type,
            self.channel_name if self.channel_name is not None else "*",
            " ".join(user.client.nickname for user in self.users.values())]


class EndOfNames(ReplyMessage):
  NAME = "366"
  FORCE_TRAILING = True

  def __init__(self, channel_name=None):
    self.channel_name = channel_name

  def as_reply_params(self, client):
    return [self.channel_name if self.channel_name is not None else "*",
            "End of /NAMES list"]
