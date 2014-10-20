import unittest

from merc import emitter


class EmitHostmaskTest(unittest.TestCase):
  def test_emit(self):
    assert emitter.emit_hostmask("a", "b", "c") == "a!b@c"


class EmitMessageTest(unittest.TestCase):
  def test_emit(self):
    assert emitter.emit_message(
        "irc.foo.org", "PRIVMSG", ["#butts", "hello world"]) == \
        ":irc.foo.org PRIVMSG #butts :hello world"

  def test_emit_no_prefix(self):
    assert emitter.emit_message(
        None, "PRIVMSG", ["#butts", "hello world"]) == \
        "PRIVMSG #butts :hello world"

  def test_emit_bad_params(self):
    with self.assertRaises(emitter.EmitterError):
      emitter.emit_message(
          "irc.foo.org", "PRIVMSG", ["#butts 2", "hello"])

  def test_emit_bad_params_with_trailing(self):
    with self.assertRaises(emitter.EmitterError):
      emitter.emit_message(
          "irc.foo.org", "PRIVMSG", ["#butts 2", "hello world"])
