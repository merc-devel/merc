import Control.Applicative
import Data.Configurator as C
import Merc.Server
import Network
import System.Log.Logger

main :: IO ()
main = do
  config <- C.load [C.Required "merc.conf"]

  updateGlobalLogger rootLoggerName (setLevel DEBUG)

  serverName <- C.require config "serverName"
  networkName <- C.require config "networkName"
  motd <- C.require config "motd"
  port <- toEnum <$> C.require config "port"

  s <- newServer serverName networkName motd
  runServer s (PortNumber port)
