import argparse
import getpass
import passlib.context


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument("--scheme", "-s", default="pbkdf2_sha256",
                    help="password scheme to use")
parser.add_argument("--stdin", "-i", action="store_true",
                    help="read the password from stdin instead of getpass")


def main():
  args = parser.parse_args()
  crypt_context = passlib.context.CryptContext(schemes=[args.scheme])

  if args.stdin:
    password = input()
  else:
    password = getpass.getpass()

  print(crypt_context.encrypt(password))
