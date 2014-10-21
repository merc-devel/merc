import coloredlogs
import logging
import yaml

from merc import server

coloredlogs.install(level=logging.INFO)
logging.getLogger("asyncio").setLevel(logging.WARN)

server.Server("merc.conf").start()
