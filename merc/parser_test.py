import unittest

from merc import parser


class ParseHostmaskTest(unittest.TestCase):
  def test_parse(self):
    nickname, username, host = parser.parse_hostmask("a!b@c")

  def test_parse_no_nickname(self):
    with self.assertRaises(parser.ParseError):
      parser.parse_hostmask("!b@c")

  def test_parse_no_username(self):
    with self.assertRaises(parser.ParseError):
      parser.parse_hostmask("a!@c")

  def test_parse_no_host(self):
    with self.assertRaises(parser.ParseError):
      parser.parse_hostmask("a!b@")

  def test_parse_no_bang(self):
    with self.assertRaises(parser.ParseError):
      parser.parse_hostmask("ab@c")

  def test_parse_no_at(self):
    with self.assertRaises(parser.ParseError):
      parser.parse_hostmask("a!bc")

  def test_parse_no_sigils(self):
    with self.assertRaises(parser.ParseError):
      parser.parse_hostmask("abc")

  def test_parse_empty(self):
    with self.assertRaises(parser.ParseError):
      parser.parse_hostmask("")


class ParseMessageTest(unittest.TestCase):
  def test_parse(self):
    prefix, command, params = parser.parse_message(
        ":irc.buttnet.org PRIVMSG #butts :hello world")
    assert prefix == "irc.buttnet.org"
    assert command == "PRIVMSG"
    assert params == ["#butts", "hello world"]

  def test_parse_no_trailing(self):
    prefix, command, params = parser.parse_message(
        ":irc.buttnet.org PRIVMSG #butts hello")
    assert prefix == "irc.buttnet.org"
    assert command == "PRIVMSG"
    assert params == ["#butts", "hello"]

  def test_parse_lowercase_command(self):
    prefix, command, params = parser.parse_message(
        ":irc.buttnet.org privmsg #butts :hello world")
    assert prefix == "irc.buttnet.org"
    assert command == "PRIVMSG"
    assert params == ["#butts", "hello world"]

  def test_parse_no_params(self):
    prefix, command, params = parser.parse_message(":irc.buttnet.org LUSERS")
    assert prefix == "irc.buttnet.org"
    assert command == "LUSERS"
    assert params == []

  def test_parse_trailing_params_only(self):
    prefix, command, params = parser.parse_message(
        ":irc.buttnet.org PING :some text here")
    assert prefix == "irc.buttnet.org"
    assert command == "PING"
    assert params == ["some text here"]

  def test_parse_no_prefix(self):
    prefix, command, params = parser.parse_message(
        "PRIVMSG #butts :hello world")
    assert prefix is None
    assert command == "PRIVMSG"
    assert params == ["#butts", "hello world"]

  def test_parse_command_only(self):
    prefix, command, params = parser.parse_message("LUSERS")
    assert prefix is None
    assert command == "LUSERS"
    assert params == []

  def test_parse_empty(self):
    with self.assertRaises(parser.ParseError):
      parser.parse_message("")

  def test_parse_emptpyish(self):
    with self.assertRaises(parser.ParseError):
      parser.parse_message("    ")
