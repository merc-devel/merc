import coloredlogs
import logging
import yaml

from merc import server

coloredlogs.install(level=logging.INFO)
logging.getLogger("asyncio").setLevel(logging.WARN)

with open("merc.conf") as f:
  config = yaml.load(f)

server.start(config)
