import unittest
from unittest import mock

from merc import message
from merc import net


class ProtocolTest(unittest.TestCase):
  def setUp(self):
    self.mock_client = mock.Mock()
    self.mock_server = mock.Mock()
    self.mock_transport = mock.Mock()

    self.mock_server.new_client = mock.Mock(return_value=self.mock_client)
    self.protocol = net.Protocol(self.mock_server)
    self.protocol.connection_made(self.mock_transport)

  def test_handles_messages_correctly(self):
    self.mock_client.on_message = mock.Mock()

    self.protocol.data_received(b"PRIVMSG #butts :this is ")
    self.protocol.data_received(b"a test\r\nNOTICE #butts ")
    (prefix, m), _ = self.mock_client.on_message.call_args
    assert prefix is None
    assert isinstance(m, message.Privmsg)
    assert m.channel == "#butts"
    assert m.text == "this is a test"

    self.protocol.data_received(b":please continue\r\n")
    (prefix, m), _ = self.mock_client.on_message.call_args
    assert prefix is None
    assert isinstance(m, message.Notice)
    assert m.channel == "#butts"
    assert m.text == "please continue"
