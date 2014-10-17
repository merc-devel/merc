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
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import qualified Merc.Parser as P
import qualified Merc.Types.Message as M
import qualified Merc.Types.Server as S
import qualified Merc.Types.User as U
import Merc.User
import Network
import System.IO
import System.Log.Logger

handleMessage :: S.Client -> S.Server -> M.Message -> IO Bool
handleMessage client@S.Client{..} server message@M.Message{..} = case command of
  M.Nick -> handleNickMessage client server params
  M.User -> handleUserMessage client server params

  M.UnknownCommand name ->
    return True

runClient :: S.Client -> S.Server -> IO ()
runClient client@S.Client{..} server = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  race receive loop
  return ()

  where
    receive = forever $ do
      line <- T.hGetLine handle
      case A.parseOnly P.message line of
        Left error -> infoM "Merc.Server" $ "Error parsing message: " ++ error
        Right message -> do
          debugM "Merc.Message" $ "Received message: " ++ show message
          atomically $ writeTChan chan message

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
      U.nickname = U.UnregisteredNickname,
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

    when (U.registered user) $ do
      modifyTVar (S.clients server) $ \clients -> do
        Map.delete (U.normalizeNickname (U.nickname (U.hostmask user))) clients

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
    forkFinally (runClient client server) $ \e ->
      infoM "Merc.Server" $ "Lost connection from " ++ host ++ " on port " ++
                            show port ++ ": " ++ show e
    loop

newServer :: T.Text -> T.Text -> IO S.Server
newServer = S.newServer
