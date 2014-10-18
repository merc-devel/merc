module Merc.User (
  handleNickMessage,
  handleUserMessage,
  handleLUsersMessage
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text as T
import Data.Time.Clock
import Merc.Message
import qualified Merc.Parser as P
import qualified Merc.Types.Channel as C
import qualified Merc.Types.Message as M
import qualified Merc.Types.Server as S
import qualified Merc.Types.User as U
import System.Log.Logger

willBeRegistered :: U.User -> Bool
willBeRegistered U.User{U.hostmask = U.Hostmask{..}, U.registered = registered} =
  case nickname of
    U.UnregisteredNickname -> False
    U.Nickname { U.unwrapName = _ } -> username /= "*" && not registered

register :: S.Client -> S.Server -> IO ()
register client@S.Client{..} server@S.Server{..} = join $ atomically $ do
  modifyTVar user $ \user -> user {
    U.registered = True
  }
  U.User{U.hostmask = U.Hostmask{U.nickname = U.Nickname nickname}} <- readTVar user

  modifyTVar clients $ \clients ->
    Map.insert (U.normalizeNickname (U.Nickname nickname)) client clients

  welcome <- rplWelcome client server
  yourHost <- rplYourHost client server
  created <- rplCreated client server
  myInfo <- rplMyInfo client server
  iSupport <- rplISupport client server

  return $ do
    sendMessage client welcome
    sendMessage client yourHost
    sendMessage client created
    sendMessage client myInfo
    sendMessage client iSupport
    _ <- handleLUsersMessage client server
    return ()

handleLUsersMessage :: S.Client -> S.Server -> IO Bool
handleLUsersMessage client server = join $ atomically $ do
  lUserClient <- rplLUserClient client server
  lUserOp <- rplLUserOp client server
  lUserUnknown <- rplLUserUnknown client server
  lUserChannels <- rplLUserChannels client server
  lUserMe <- rplLUserMe client server

  return $ do
    sendMessage client lUserClient
    sendMessage client lUserOp
    sendMessage client lUserUnknown
    sendMessage client lUserChannels
    sendMessage client lUserMe
    return True

handleNickMessage :: S.Client -> S.Server -> [T.Text] -> IO Bool
handleNickMessage client@S.Client{..} server params = do
  case params of
    (nickname:_) -> case A.parseOnly P.nickname nickname of
        Left _ -> atomically (errErroneousNickname client server) >>= sendMessage client
        Right nickname -> join $ atomically $ do
          clients <- readTVar $ S.clients server
          U.User{U.hostmask = hostmask@U.Hostmask{U.nickname = oldNickname}, U.registered = registered} <- readTVar user

          let nickname' = U.normalizeNickname nickname
          let oldNickname' = U.normalizeNickname oldNickname

          case Map.lookup nickname' clients of
            Just _ -> do
              e <- errNicknameInUse client server (U.unwrapName nickname)
              return $ case oldNickname of
                U.Nickname{..} | nickname' == oldNickname' ->
                  return ()
                _ -> sendMessage client e

            Nothing -> do
              modifyTVar user $ \user -> user {
                U.hostmask = (U.hostmask user) {
                  U.nickname = nickname
                }
              }

              when registered $ modifyTVar (S.clients server) $ \clients -> do
                Map.insert nickname' client $ Map.delete oldNickname' clients

              user <- readTVar user
              return $ do
                when (willBeRegistered user) $ register client server
                when registered $ do
                  sendMessage client M.Message {
                    M.prefix = Just (M.HostmaskPrefix hostmask),
                    M.command = M.Nick,
                    M.params = [U.unwrapName nickname]
                  }
    _ -> atomically (errNeedMoreParams client server M.Nick) >>= sendMessage client
  return True

handleUserMessage :: S.Client -> S.Server -> [T.Text] -> IO Bool
handleUserMessage client@S.Client{..} server params = do
  case params of
    (username:mode:_:realname:_) -> join $ atomically $ do
      modifyTVar user $ \user -> user {
        U.realname = realname,
        U.hostmask = (U.hostmask user) {
          U.username = username
        }
      }

      user <- readTVar user

      return $ when (willBeRegistered user) (register client server)
    _ -> atomically (errNeedMoreParams client server M.User) >>= sendMessage client
  return True
