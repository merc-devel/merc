module Merc.Server (
  newServer,
  runServer
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Attoparsec.Text as A
import qualified Data.Bimap as B
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

willBeRegistered :: U.User -> Bool
willBeRegistered U.User{U.hostmask = U.Hostmask{..}, U.registered = registered} =
  U.unwrapName nickname /= "*" && username /= "*" && not registered

welcome :: S.Client -> S.Server -> IO ()
welcome S.Client{..} server@S.Server{..} = join $ atomically $ do
  modifyTVar user $ \user -> user {
    U.registered = True
  }
  U.User{U.hostmask = U.Hostmask{U.nickname = U.Nickname nickname}} <- readTVar user

  return $ do
    send M.RplWelcome [nickname, "Welcome to the " <> networkName <> " Internet Relay Chat Network " <> nickname]
    send M.RplYourHost [nickname, "Your host is " <> serverName <> ", running mercd-master"]
    send M.RplCreated [nickname, "This server was created " <> T.pack (formatTime defaultTimeLocale "%c" creationTime)]

  where
    send command params =
      T.hPutStrLn handle $ E.emitMessage $ newServerMessage server command params

newServerMessage :: S.Server -> M.Command -> [T.Text] -> M.Message
newServerMessage S.Server{S.serverName = serverName} command params = M.Message {
  M.prefix = Just $ M.ServerPrefix serverName,
  M.command = command,
  M.params = params
}
  where
    prefix = Just $ M.ServerPrefix serverName

handleMessage :: S.Client -> S.Server -> M.Message -> IO Bool
handleMessage client@S.Client{..} server message@M.Message{..} = case command of
  M.Nick -> do
    case params of
      (nickname:_) -> join $ atomically $ do
        modifyTVar user $ \user -> user {
          U.hostmask = (U.hostmask user) {
            U.nickname = U.Nickname nickname
          }
        }

        user <- readTVar user
        return $ when (willBeRegistered user) (welcome client server)
      _ -> needMoreParams
    return True

  M.User -> do
    case params of
      (username:mode:_:realname:_) -> join $ atomically $ do
        modifyTVar user $ \user -> user {
          U.realname = realname,
          U.hostmask = (U.hostmask user) {
            U.username = username
          }
        }

        user <- readTVar user
        return $ when (willBeRegistered user) (welcome client server)
      _ -> needMoreParams
    return True

  M.UnknownCommand name ->
    return True

  where
    needMoreParams = join $ atomically $ do
      U.User{U.hostmask = U.Hostmask{U.nickname = U.Nickname nickname}} <- readTVar user
      let commandName = fromJust $ B.lookup command M.commandNames
      return $ T.hPutStrLn handle $ E.emitMessage $ newServerMessage server M.ErrNeedMoreParams [nickname, commandName, "Not enough parameters"]

runClient :: S.Client -> S.Server -> IO ()
runClient client@S.Client{..} server = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  race receive loop
  return ()

  where
    sendMessage message = writeTChan chan message

    receive = forever $ do
      line <- T.hGetLine handle
      case A.parseOnly P.message line of
        Left error -> infoM "Merc.Server" $ "Error parsing message: " ++ error
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
      U.nickname = U.Nickname "*",
      U.username = "*",
      U.host = T.pack host
    },
    U.realname = "",
    U.realHost = host,
    U.connectionTime = now,
    U.lastActiveTime = now,
    U.registered = False
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

    when (U.unwrapName (U.nickname hostmask) /= "*") $ do
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
