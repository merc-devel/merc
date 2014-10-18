module Merc.Server (
  newServer,
  runServer
) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Attoparsec.Text as A
import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as S
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import qualified Merc.Parser as P
import qualified Merc.Types.Message as M
import qualified Merc.Types.Server as S
import qualified Merc.Types.User as U
import Merc.Message
import Merc.User
import Network
import System.IO
import System.Log.Logger

handleMessage :: S.Client -> S.Server -> M.Message -> IO Bool
handleMessage client@S.Client{..} server message@M.Message{..} = join $ atomically $ do
  u <- readTVar user
  let handler = if U.registered u then handleRegisteredMessage
                                  else handleUnregisteredMessage

  return $ handler client server message

handleUnregisteredMessage client server message@M.Message{..} = case command of
  M.Nick -> handleNickMessage client server params
  M.User -> handleUserMessage client server params
  _ -> return True

handleRegisteredMessage client server message@M.Message{..} = case command of
  M.Nick -> handleNickMessage client server params
  M.User -> do
    e <- atomically $ errAlreadyRegistered client server
    sendMessage client e
    return True
  M.UnknownCommand command -> do
    e <- atomically $ errUnknownCommand client server command
    sendMessage client e
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
        Left error ->
          infoM "Merc.Server" $ "Error parsing message: " ++ error
        Right message -> do
          debugM "Merc.Server" $ "Received message: " ++ show message
          atomically $ writeTChan chan message

    loop = join $ atomically $ do
      message <- readTChan chan
      return $ do
        continue <- handleMessage client server message
        when continue $ loop

newUser :: HostName -> IO U.User
newUser host = do
  now <- getCurrentTime

  return $ U.User {
    U.hostmask = U.Hostmask {
      U.nickname = U.UnregisteredNickname,
      U.username = "*",
      U.host = T.pack host
    },
    U.realname = "",
    U.realHost = host,
    U.connectionTime = now,
    U.lastActiveTime = now,
    U.registered = False,
    U.channels = S.empty
  }

newClient :: Handle -> HostName -> IO S.Client
newClient handle host = do
  u <- newUser host >>= newTVarIO
  c <- newTChanIO

  return S.Client {
    S.user = u,
    S.handle = handle,
    S.chan = c
  }

closeClient :: S.Client -> S.Server -> IO ()
closeClient client@S.Client{..} server@S.Server{..} = do
  hClose handle

  join $ atomically $ do
    U.User {U.hostmask = U.Hostmask{U.nickname = nickname}, U.registered = registered} <- readTVar user

    when registered $ do
      modifyTVar clients $ \clients -> do
        Map.delete (U.normalizeNickname nickname) clients

    return $ do
      debugM "Merc.Server" $ "Deleted user " ++ show nickname

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
    forkFinally (runClient client server) $ \e -> do
      infoM "Merc.Server" $ "Lost connection from " ++ host ++ " on port " ++
                            show port ++ ": " ++ show e
      closeClient client server
    loop

newServer :: T.Text -> T.Text -> IO S.Server
newServer = S.newServer
