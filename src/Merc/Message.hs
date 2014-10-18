module Merc.Message (
  sendMessage,
  pong,
  errNeedMoreParams,
  errErroneousNickname,
  errNicknameInUse,
  errUnknownCommand,
  errAlreadyRegistered,
  rplWelcome,
  rplYourHost,
  rplCreated,
  rplMyInfo,
  rplISupport,
  rplLUserClient,
  rplLUserOp,
  rplLUserUnknown,
  rplLUserChannels,
  rplLUserMe
) where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Format
import Merc.Emitter
import qualified Merc.Types.Channel as C
import qualified Merc.Types.Message as M
import qualified Merc.Types.Server as S
import qualified Merc.Types.User as U
import Merc.Util
import Network
import System.Locale
import System.Log.Logger

newServerMessage :: S.Server -> M.Command -> [T.Text] -> M.Message
newServerMessage S.Server{..} command params = M.Message {
  M.prefix = Just $ M.ServerPrefix serverName,
  M.command = command,
  M.params = params
}

newReplyMessage :: S.Client -> S.Server -> M.Command -> [T.Text] -> STM M.Message
newReplyMessage S.Client{..} server command params = do
  U.User{U.hostmask = U.Hostmask{U.nickname = nickname}} <- readTVar user
  return $ newServerMessage server command ((U.showNickname nickname):params)

sendMessage :: S.Client -> M.Message -> IO ()
sendMessage S.Client{..} message = do
  debugM "Merc.Message" $ "Sending message: " ++ show message
  T.hPutStrLn handle $ T.take M.maxMessageLength $ emitMessage message

pong :: S.Client -> S.Server -> T.Text -> T.Text -> STM M.Message
pong client server serverName value = do
  newReplyMessage client server M.Pong [serverName, value]

errNeedMoreParams :: S.Client -> S.Server -> M.Command -> STM M.Message
errNeedMoreParams client server command =
  newReplyMessage client server M.ErrNeedMoreParams [commandName,
                                                     "Not enough parameters"]

  where
    commandName = fromJust $ M.getCommandName command

errErroneousNickname :: S.Client -> S.Server -> STM M.Message
errErroneousNickname client server = do
  newReplyMessage client server M.ErrErroneousNickname ["Erroneous nickname"]

errNicknameInUse :: S.Client -> S.Server -> T.Text -> STM M.Message
errNicknameInUse client server nickname =
  newReplyMessage client server M.ErrNicknameInUse [nickname, "Nickname is already in use"]

errAlreadyRegistered :: S.Client -> S.Server -> STM M.Message
errAlreadyRegistered client server = do
  newReplyMessage client server M.ErrAlreadyRegistered ["You may not reregister"]

errUnknownCommand :: S.Client -> S.Server -> T.Text -> STM M.Message
errUnknownCommand client server command =
  newReplyMessage client server M.ErrUnknownCommand [command, "Unknown command"]

rplWelcome :: S.Client -> S.Server -> STM M.Message
rplWelcome client@S.Client{..} server@S.Server{..} = do
  U.User{U.hostmask = U.Hostmask{U.nickname = nickname}} <- readTVar user

  newReplyMessage client server M.RplWelcome ["Welcome to the " <>
                                              networkName <>
                                              " Internet Relay Chat Network " <>
                                              U.showNickname nickname]

rplYourHost :: S.Client -> S.Server -> STM M.Message
rplYourHost client server@S.Server{..} =
  newReplyMessage client server M.RplYourHost ["Your host is " <> serverName <>
                                               ", running " <> mercVersion]

rplCreated :: S.Client -> S.Server -> STM M.Message
rplCreated client server@S.Server{..} =
  newReplyMessage client server M.RplCreated [
    "This server was created " <> T.pack (formatTime defaultTimeLocale "%c"
                                                     creationTime)]

rplMyInfo :: S.Client -> S.Server -> STM M.Message
rplMyInfo client server@S.Server{..} =
  newReplyMessage client server M.RplMyInfo [
      serverName, mercVersion,
      T.pack $ map U.unwrapUserMode $ S.toList U.userModes,
      T.pack $ map C.unwrapChannelMode $ S.toList C.channelModes,
      T.pack $ map C.unwrapChannelMode $ S.toList C.channelModesWithParams]

rplISupport :: S.Client -> S.Server -> STM M.Message
rplISupport client server =
  newReplyMessage client server M.RplISupport [
      T.intercalate " " textParameters, "are supported by this server"]
  where
    iSupportParameters = Map.fromList [
      (S.Prefix, Just ("(" <> T.pack roleModeChars <> ")" <> T.pack rolePrefixChars)),
      (S.Charset, Just "UTF-8")]
      where
        roleModeChars = map C.unwrapChannelMode C.roleModes
        rolePrefixChars = map C.unwrapUserPrefix C.rolePrefixes

    textParameters = map (uncurry toTextParameter) (Map.toList iSupportParameters)
    toTextParameter k maybeV = case maybeV of
      Just v -> textParameterName <> "=" <> v
      Nothing -> textParameterName
      where
        textParameterName = fromJust $ S.getISupportTokenName k

rplLUserClient :: S.Client -> S.Server -> STM M.Message
rplLUserClient client server@S.Server{..} = do
  clients <- readTVar clients

  newReplyMessage client server M.RplLUserUnknown [
    "There are " <> T.pack (show (Map.size clients)) <> " users and 0 invisible on 1 servers"]

rplLUserOp :: S.Client -> S.Server -> STM M.Message
rplLUserOp client server =
  newReplyMessage client server M.RplLUserOp ["0", "IRC Operators online"]

rplLUserUnknown :: S.Client -> S.Server -> STM M.Message
rplLUserUnknown client server =
  newReplyMessage client server M.RplLUserUnknown ["0", "unknown connection(s)"]

rplLUserChannels :: S.Client -> S.Server -> STM M.Message
rplLUserChannels client server@S.Server{..} = do
  channels <- readTVar channels

  newReplyMessage client server M.RplLUserChannels [
    T.pack (show (Map.size channels)), "channels formed"]

rplLUserMe :: S.Client -> S.Server -> STM M.Message
rplLUserMe client server@S.Server{..} = do
  clients <- readTVar clients

  newReplyMessage client server M.RplLUserMe [
    "I have " <> T.pack (show (Map.size clients)) <> " clients and 1 servers"]
