import coloredlogs
import logging
from merc import server

coloredlogs.install(level=logging.WARN)
server.start(server.make_config_parser().parse_args())
