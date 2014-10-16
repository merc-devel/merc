module Merc.Server (
  newServer,
  runServer
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Attoparsec.Text as A
import Data.Function
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Time.Format
import qualified Merc.Emitter as E
import qualified Merc.Parser as P
import qualified Merc.Types.Message as M
import qualified Merc.Types.Server as S
import qualified Merc.Types.User as U
import Network
import System.IO
import System.Locale
import System.Log.Logger

isWelcome :: U.User -> Bool
isWelcome U.User{U.hostmask = U.Hostmask{U.nickname = maybeNickname, U.username = maybeUsername}} =
  isJust maybeNickname && isJust maybeUsername

welcome :: S.Client -> S.Server -> IO ()
welcome S.Client{S.handle = handle, S.user = user} server@S.Server{S.serverName = serverName, S.networkName = networkName, S.creationTime = creationTime} = join $ atomically $ do
  U.User{U.hostmask = U.Hostmask{U.nickname = Just (U.Nickname nickname)}} <- readTVar user

  return $ do
    T.hPutStrLn handle $ E.emitMessage $ newServerMessage server M.RplWelcome [
      nickname, "Welcome to the " <> networkName <> " Internet Relay Chat Network " <> nickname]
    T.hPutStrLn handle $ E.emitMessage $ newServerMessage server M.RplYourHost [
      nickname, "Your host is " <> serverName <> ", running mercd-master"]
    T.hPutStrLn handle $ E.emitMessage $ newServerMessage server M.RplCreated [
      nickname, "This server was created " <> T.pack (formatTime defaultTimeLocale "%c" creationTime)]

newServerMessage :: S.Server -> M.Command -> [T.Text] -> M.Message
newServerMessage S.Server{S.serverName = serverName} command params = M.Message {
  M.prefix = Just $ M.ServerPrefix serverName,
  M.command = command,
  M.params = params
}
  where
    prefix = Just $ M.ServerPrefix serverName

handleMessage :: S.Client -> S.Server -> M.Message -> IO Bool
handleMessage client@S.Client{S.handle = handle, S.user = user} server message@M.Message{M.command = command, M.params = params} = do
  case command of
    M.Nick -> do
      let [nickname] = params

      join $ atomically $ do
        modifyTVar user $ \user ->
          user {
            U.hostmask = (U.hostmask user) {
              U.nickname = Just $ U.Nickname nickname
            }
        }

        user <- readTVar user
        return $ when (isWelcome user) (welcome client server)

    M.User -> do
      let [username, mode, _, realname] = params

      join $ atomically $ do
        modifyTVar user $ \user ->
          user {
            U.realname = realname,
            U.hostmask = (U.hostmask user) {
              U.username = Just $ username
            }
        }

        user <- readTVar user
        return $ when (isWelcome user) (welcome client server)

  return True

runClient :: S.Client -> S.Server -> IO ()
runClient client server = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  race receive loop
  return ()

  where
    handle = S.handle client
    chan = S.chan client

    sendMessage message = writeTChan chan message

    receive = forever $ do
      line <- T.hGetLine handle
      case A.parseOnly P.message line of
        Left _ -> return ()
        Right message -> do
          atomically $ sendMessage message

    loop = join $ atomically $ do
      message <- readTChan chan
      return $ do
        continue <- handleMessage client server message
        when continue $ loop

newClient :: Handle -> HostName -> IO S.Client
newClient handle host = do
  now <- getCurrentTime

  u <- newTVarIO $ U.User {
    U.hostmask = U.Hostmask {
      U.nickname = Nothing,
      U.username = Nothing,
      U.host = T.pack host
    },
    U.realname = "",
    U.realHost = host,
    U.connectionTime = now,
    U.lastActiveTime = now
  }

  c <- newTChanIO

  return S.Client {
    S.user = u,
    S.handle = handle,
    S.chan = c
  }

closeClient :: S.Client -> S.Server -> IO ()
closeClient client server = do
  hClose handle

  atomically $ do
    user <- readTVar $ S.user client
    let hostmask = U.hostmask user

    when (isJust $ U.nickname hostmask) $ do
      modifyTVar (S.clients server) $ \clients -> do
        clients

  where
    handle = S.handle client

runServer :: S.Server -> IO ()
runServer server = withSocketsDo $ do
  sock <- listenOn (PortNumber 9999)
  infoM "Merc.Server" "Listening on port 9999."

  fix $ \loop -> do
    (handle, host, port) <- accept sock
    infoM "Merc.Server" $ "Accepted connection from " ++ host ++ " on port " ++
                          show port ++ "."
    client <- newClient handle host
    forkFinally (runClient client server) $ \_ -> do
      infoM "Merc.Server" $ "Lost connection from " ++ host ++ " on port " ++
                            show port ++ "."
      closeClient client server
    loop

newServer :: T.Text -> T.Text -> IO S.Server
newServer = S.newServer
