module Merc.Channel (
  joinChannel,
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

joinChannel :: S.Client -> S.Server -> C.ChannelName -> Maybe T.Text -> STM Bool
joinChannel client@S.Client{..} server@S.Server{..} channelName key = do
  U.User{U.hostmask = hostmask@U.Hostmask{U.nickname = nickname}, U.channels = userChannels} <- readTVar user

  if | not (channelName' `Set.member` userChannels) -> do
      -- TODO: support keys
      modifyTVar (S.channels server) $ \channels -> do
        let nickname' = U.normalizeNickname nickname
        let channel = Map.findWithDefault (newChannel channelName) channelName' channels

        Map.insert channelName' channel{
          C.users = Map.insert nickname' C.ChannelUser{C.role = C.NoRole} (C.users channel)
        } channels

      modifyTVar user $ \user@U.User{..} -> user{
        U.channels=Set.insert channelName' channels
      }
      return True
     | otherwise -> return False

  where
    channelName' = C.normalizeChannelName channelName

partChannel :: S.Client -> S.Server -> C.ChannelName -> STM Bool
partChannel client@S.Client{..} server@S.Server{..} channelName = do
  U.User{U.hostmask = hostmask@U.Hostmask{U.nickname = nickname}, U.channels = userChannels} <- readTVar user

  if | channelName' `Set.member` userChannels -> do
      modifyTVar (S.channels server) $ \channels -> do
        let nickname' = U.normalizeNickname nickname
        let channel = Map.findWithDefault (newChannel channelName) channelName' channels
        let channel' = channel{
          C.users = Map.delete nickname' (C.users channel)
        }

        let channels' = Map.insert channelName' channel' channels

        if Map.null (C.users channel') then Map.delete channelName' channels' else channels'

      modifyTVar user $ \user@U.User{..} -> user{
        U.channels=Set.delete channelName' channels
      }
      return True
     | otherwise -> return False

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
    joinChannel' channelName key = join $ atomically $ do
      didJoin <- joinChannel client server channelName key
      joinMessage <- M.join client server channelName
      return $ when didJoin (sendMessage client joinMessage)

    joinChannels channels keys =
      mapM_ (uncurry joinChannel') $ zip channels (map Just keys ++ repeat Nothing)
