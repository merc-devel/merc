module Merc.Types.Server (
  Server(..),
  Client(..),
  newServer
) where

import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.Clock
import qualified Merc.Types.Channel as C
import qualified Merc.Types.Message as M
import qualified Merc.Types.User as U
import Network
import System.IO

data Client = Client {
  user :: TVar U.User,
  handle :: Handle,
  chan :: TChan M.Message
}

data Server = Server {
  serverName :: T.Text,
  networkName :: T.Text,
  clients :: TVar (M.Map U.NormalizedNickname Client),
  channels :: TVar (M.Map C.NormalizedChannelName C.Channel),
  creationTime :: UTCTime
}

newServer :: T.Text -> T.Text -> IO Server
newServer serverName networkName = do
  clients <- newTVarIO M.empty
  channels <- newTVarIO M.empty

  now <- getCurrentTime

  return Server {
    serverName = serverName,
    networkName = networkName,
    clients = clients,
    channels = channels,
    creationTime = now
  }
