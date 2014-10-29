from os import path

from merc import config
from merc import util


class TLS(config.Section):
  cert = config.constrained(str, lambda x: (path.isfile(x), "must exist"))
  key = config.constrained(str, lambda x: (path.isfile(x), "must exist"))

class Bind(config.Section):
  host = config.optional(str, "0.0.0.0")
  port = int
  tls = config.optional(bool, False)
  type = config.optional(config.any(('servers', 'users')), "users")

class Link(config.Section):
  host = str
  port = int
  send_password = str
  receive_password = str
  tls = config.optional(bool, False)
  hub = config.optional(bool, False)
  services = config.optional(bool, False)

class Oper(config.Section):
  password = str
  hostmasks = [str]

class Config(config.Section):
  class server(config.Section):
    name = str
    network_name = str
    sid = config.constrained(str, lambda x: (util.is_sid(x), "not in SID format"))

  motd = str

  class admin(config.Section):
    name = str
    email = str
    location = str
    location_fine = str

  features = [str]

  tls = config.optional(TLS)
  class crypto(config.Section):
    hash_schemes = [str]

  bind = [Bind]
  opers = {str: Oper}
  links = {str: Link}
