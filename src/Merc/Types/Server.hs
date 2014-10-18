module Merc.Types.Server (
  Server(..),
  Client(..),
  ISupportToken(..),
  getISupportTokenName
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
  motd :: T.Text,
  clients :: TVar (M.Map U.NormalizedNickname Client),
  channels :: TVar (M.Map C.NormalizedChannelName C.Channel),
  creationTime :: UTCTime
}

data ISupportToken = Prefix
                   | Charset
                   deriving (Eq, Ord, Show)

iSupportTokens :: M.Map ISupportToken T.Text
iSupportTokens = M.fromList [
  (Prefix, "PREFIX"),
  (Charset, "CHARSET")]

getISupportTokenName :: ISupportToken -> Maybe T.Text
getISupportTokenName parameter = M.lookup parameter iSupportTokens
