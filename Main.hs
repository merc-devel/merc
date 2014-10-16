import Merc.Server
import System.Log.Logger

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO)

  s <- newServer "irc.buttnet.org" "buttnet"
  runServer s
