module Merc.Channel (
  joinChannel,
  partChannel,
  partChannel',
  handleJoinMessage,
  handlePartMessage,
  handleNamesMessage
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import Merc.Message
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

joinChannel :: S.Client -> S.Server -> C.ChannelName -> Maybe T.Text -> STM (Maybe C.Channel)
joinChannel client@S.Client{..} server@S.Server{..} channelName key = do
  U.User{U.hostmask = hostmask@U.Hostmask{U.nickname = nickname}, U.channels = userChannels} <- readTVar user

  if | not (channelName' `Set.member` userChannels) -> do
      -- TODO: support keys
      channels <- readTVar channels
      let channel = Map.findWithDefault (newChannel channelName) channelName' channels

      let nickname' = U.normalizeNickname nickname
      let channel' = channel{
          C.users = Map.insert nickname' C.ChannelUser{C.role = C.NoRole} (C.users channel)
        }

      modifyTVar (S.channels server) $ \channels -> do
        Map.insert channelName' channel' channels

      modifyTVar user $ \user@U.User{..} -> user{
        U.channels=Set.insert channelName' channels
      }
      return $ Just channel'
     | otherwise -> return Nothing

  where
    channelName' = C.normalizeChannelName channelName

partChannel :: S.Client -> S.Server -> C.ChannelName -> STM (Maybe C.Channel)
partChannel client server channelName =
  partChannel' client server (C.normalizeChannelName channelName)

partChannel' :: S.Client -> S.Server -> C.NormalizedChannelName -> STM (Maybe C.Channel)
partChannel' client@S.Client{..} server@S.Server{..} channelName' = do
  U.User{U.hostmask = hostmask@U.Hostmask{U.nickname = nickname}, U.channels = userChannels} <- readTVar user

  if | channelName' `Set.member` userChannels -> do
      channels <- readTVar channels
      case Map.lookup channelName' channels of
        Nothing -> return Nothing
        Just channel -> do
          let nickname' = U.normalizeNickname nickname
          let channel' = channel{
            C.users = Map.delete nickname' (C.users channel)
          }

          modifyTVar (S.channels server) $ \channels -> do
            let channels' = Map.insert channelName' channel' channels
            if Map.null (C.users channel') then Map.delete channelName' channels' else channels'

          modifyTVar user $ \user@U.User{..} -> user{
            U.channels=Set.delete channelName' channels
          }

          return $ Just channel'
     | otherwise -> return Nothing

makeNamesReply :: S.Client -> S.Server -> C.ChannelName -> STM [M.Message]
makeNamesReply client server@S.Server{..} channelName = do
  clients <- readTVar clients
  channels <- readTVar channels

  case Map.lookup channelName' channels of
    Nothing -> return []
    Just C.Channel{C.users = users, C.name = channelName} -> do
      userRoles <- sequence $ do
        (nickname', channelUser) <- Map.toList users
        return $ do
          nickname <- realNicknameFor clients nickname'
          return $ (nickname, C.role channelUser)

      names <- rplNameReply client server userRoles
      end <- rplEndOfNames client server channelName
      return [names, end]

  where
    channelName' = C.normalizeChannelName channelName
    realNicknameFor clients nickname' = do
      U.User{U.hostmask = U.Hostmask{U.nickname = nickname}} <- readTVar $ S.user (fromJust (Map.lookup nickname' clients))
      return nickname

handleJoinMessage :: S.Client -> S.Server -> [T.Text] -> IO Bool
handleJoinMessage client server params = do
  case params of
    (rawChannels:rawKeys:_) ->
      joinRawChannels (T.split (==',') rawChannels) (T.split (==',') rawKeys)

    (rawChannels:_) ->
      joinRawChannels (T.split (==',') rawChannels) []

    _ ->
      atomically (errNeedMoreParams client server M.Join) >>= sendMessage client
  return True
  where
    joinRawChannel rawChannelName key = case A.parseOnly P.channelName rawChannelName of
      Left _ ->
        atomically (errNoSuchChannel client server rawChannelName) >>= sendMessage client
      Right channelName -> join $ atomically $ do
        maybeChannel <- joinChannel client server channelName key

        case maybeChannel of
          Nothing -> return $ return ()
          Just C.Channel{C.name = channelName} -> do
            joinMessage <- cmdJoin client server channelName
            namesReply <- makeNamesReply client server channelName

            return $ do
              broadcastMessageToChannel server channelName joinMessage
              mapM_ (sendMessage client) namesReply

    joinRawChannels rawChannels keys =
      mapM_ (uncurry joinRawChannel) $ zip rawChannels (map Just keys ++ repeat Nothing)

handlePartMessage :: S.Client -> S.Server -> [T.Text] -> IO Bool
handlePartMessage client server params = do
  case params of
    (rawChannels:reason:_) ->
      partRawChannels reason (T.split (==',') rawChannels)

    (rawChannels:_) ->
      partRawChannels "" (T.split (==',') rawChannels)

    _ ->
      atomically (errNeedMoreParams client server M.Part) >>= sendMessage client
  return True
  where
    partRawChannel reason rawChannelName = case A.parseOnly P.channelName rawChannelName of
      Left _ ->
        atomically (errNoSuchChannel client server rawChannelName) >>= sendMessage client
      Right channelName -> join $ atomically $ do
        maybeChannel <- partChannel client server channelName

        case maybeChannel of
          Nothing -> do
            notOnChannelMessage <- errNotOnChannel client server rawChannelName
            return $ sendMessage client notOnChannelMessage
          Just C.Channel{C.name = channelName} -> do
            partMessage <- cmdPart client server channelName reason
            return $ sendMessage client partMessage

    partRawChannels reason rawChannels =
      mapM_ (partRawChannel reason) rawChannels

handleNamesMessage :: S.Client -> S.Server -> [T.Text] -> IO Bool
handleNamesMessage client server params = do
  case params of
    (rawChannels:_) ->
      mapM_ namesRawChannel (T.split (==',') rawChannels)

    _ ->
      atomically (errNeedMoreParams client server M.Names) >>= sendMessage client
  return True
  where
    namesRawChannel rawChannelName = case A.parseOnly P.channelName rawChannelName of
      Left _ ->
        atomically (errNoSuchChannel client server rawChannelName) >>= sendMessage client
      Right channelName -> join $ atomically $ do
        reply <- makeNamesReply client server channelName
        return $ mapM_ (sendMessage client) reply
