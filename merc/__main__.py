import argparse
import coloredlogs
import logging
import yaml

from merc import server

coloredlogs.install(level=logging.INFO)
logging.getLogger("asyncio").setLevel(logging.WARN)

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument("--config", "-c", help="file to load configuration from",
                    default="merc.conf")

args = parser.parse_args()

server.Server(args.config).start()
