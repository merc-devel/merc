module Merc.Channel (
  handleJoinMessage
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Merc.Message hiding (join)
import qualified Merc.Message as M
import qualified Merc.Parser as P
import qualified Merc.Types.Channel as C
import qualified Merc.Types.Message as M
import qualified Merc.Types.Server as S
import qualified Merc.Types.User as U

channelList :: A.Parser [C.ChannelName]
channelList = P.channelName `A.sepBy1` (A.char ',')

newChannel :: C.ChannelName -> C.Channel
newChannel channelName = C.Channel {
  C.name = channelName,
  C.users = Map.empty,
  C.topic = ""
}

joinChannel :: S.Client -> S.Server -> C.ChannelName -> Maybe T.Text -> IO ()
joinChannel client@S.Client{..} server@S.Server{..} channelName key = join $ atomically $ do
  U.User{U.hostmask = hostmask@U.Hostmask{U.nickname = nickname}, U.channels = userChannels} <- readTVar user
  let nickname' = U.normalizeNickname nickname

  -- TODO: support keys
  modifyTVar (S.channels server) $ \channels -> do
    let channel = Map.findWithDefault (newChannel channelName) channelName' channels
    Map.insert channelName' channel{
      C.users = Map.insert nickname' C.ChannelUser{C.role = C.NoRole} (C.users channel)
    } channels

  modifyTVar user $ \user@U.User{..} -> user{U.channels=Set.insert channelName' channels}

  return $ atomically (M.join client server channelName) >>= sendMessage client

  where
    channelName' = C.normalizeChannelName channelName

handleJoinMessage :: S.Client -> S.Server -> [T.Text] -> IO Bool
handleJoinMessage client server params = do
  case params of
    (rawChannels:rawKeys:_) -> do
      let Right channels = A.parseOnly channelList rawChannels
      let keys = T.split (==',') rawKeys
      joinChannels channels keys

    (rawChannels:_) -> do
      let Right channels = A.parseOnly channelList rawChannels
      joinChannels channels []

    _ -> atomically (errNeedMoreParams client server M.Nick) >>= sendMessage client
  return True
  where
    joinChannels channels keys =
      mapM_ (uncurry (joinChannel client server)) $ zip channels (map Just keys ++ repeat Nothing)
