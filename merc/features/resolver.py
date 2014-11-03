import aiodns
import asyncio
import ipaddress

from merc import feature


class ResolverFeature(feature.Feature):
  NAME = __name__


install = ResolverFeature.install


@asyncio.coroutine
def resolve_hostname_coro(app, user, timeout):
  resolver = aiodns.DNSResolver(loop=app.loop)

  host, *_ = user.protocol.transport.get_extra_info("peername")
  host, _, _ = host.partition("%")

  app.run_hooks("server.notify", user,
                   "*** Looking up your hostname...")
  ip = ipaddress.ip_address(host)

  is_ipv4 = False

  if isinstance(ip, ipaddress.IPv4Address):
    rip = ".".join(reversed(ip.exploded.split("."))) + ".in-addr.arpa."
    is_ipv4 = True
  elif isinstance(ip, ipaddress.IPv6Address):
    rip = ".".join(reversed("".join(ip.exploded.split(":")))) + ".ip6.arpa."

  try:
    forward, *_ = yield from asyncio.wait_for(resolver.query(rip, "PTR"),
                                              timeout)
    backward, *_ = yield from asyncio.wait_for(resolver.query(
        forward, "AAAA" if not is_ipv4 else "A"), timeout)

    if ip == ipaddress.ip_address(backward):
      app.run_hooks("server.notify", user,
                       "*** Found your hostname ({})".format(forward))
      user.host = forward
    else:
      app.run_hooks("server.notify", user,
                       "*** Hostname does not resolve correctly")
  except (aiodns.error.DNSError, asyncio.TimeoutError):
    app.run_hooks("server.notify", user,
                     "*** Couldn't look up your hostname")
    user.host = host

  if user.is_ready_for_registration:
    user.register(app)


@ResolverFeature.hook("user.connect")
def resolve_hostname(app, user):
  asyncio.async(resolve_hostname_coro(app, user, 5), loop=app.loop)
