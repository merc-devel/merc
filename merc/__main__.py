import coloredlogs
from merc import server

coloredlogs.install()
server.start(server.make_config_parser().parse_args())
