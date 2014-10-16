module Merc.User (
  handleNickMessage,
  handleUserMessage
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as Map
import Data.Monoid
import Data.Time.Clock
import Data.Time.Format
import Merc.Message
import qualified Merc.Types.Message as M
import qualified Merc.Types.Server as S
import qualified Merc.Types.User as U
import qualified Data.Text as T
import System.Locale

willBeRegistered :: U.User -> Bool
willBeRegistered U.User{U.hostmask = U.Hostmask{..}, U.registered = registered} =
  U.unwrapName nickname /= "*" && username /= "*" && not registered

register :: S.Client -> S.Server -> IO ()
register client@S.Client{..} server@S.Server{..} = join $ atomically $ do
  modifyTVar user $ \user -> user {
    U.registered = True
  }
  U.User{U.hostmask = U.Hostmask{U.nickname = U.Nickname nickname}} <- readTVar user

  modifyTVar clients $ \clients ->
    Map.insert (U.normalizeNickname (U.Nickname nickname)) client clients

  return $ do
    send M.RplWelcome [nickname, "Welcome to the " <> networkName <>
                                 " Internet Relay Chat Network " <> nickname]
    send M.RplYourHost [nickname, "Your host is " <> serverName <>
                                  ", running mercd-master"]
    send M.RplCreated [nickname, "This server was created " <>
                                  T.pack (formatTime defaultTimeLocale "%c"
                                          creationTime)]

  where
    send command params = sendMessage client $ newServerMessage server command params

handleNickMessage :: S.Client -> S.Server -> [T.Text] -> IO Bool
handleNickMessage client@S.Client{..} server params = do
  case params of
    (nickname:_) -> join $ atomically $ do
      modifyTVar user $ \user -> user {
        U.hostmask = (U.hostmask user) {
          U.nickname = U.Nickname nickname
        }
      }

      user <- readTVar user
      return $ when (willBeRegistered user) (register client server)
    _ -> do
      e <- atomically (needMoreParams client server M.Nick)
      sendMessage client e
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
    _ -> do
      e <- atomically (needMoreParams client server M.User)
      sendMessage client e
  return True
