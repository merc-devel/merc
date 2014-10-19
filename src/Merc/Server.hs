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
import qualified Data.Set as Set
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import qualified Merc.Parser as P
import qualified Merc.Types.Channel as C
import qualified Merc.Types.Message as M
import qualified Merc.Types.Server as S
import qualified Merc.Types.User as U
import Merc.Channel
import Merc.Message hiding (join)
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
  M.UnknownCommand command -> return True
  _ ->
    -- TODO: tell the user they have to register
    return True

handleRegisteredMessage client server@S.Server{..} message@M.Message{..} = case command of
  M.Nick -> handleNickMessage client server params

  M.User -> do
    atomically (errAlreadyRegistered client server) >>= sendMessage client
    return True

  M.Ping -> do
    case params of
      (value:serverName:_) -> atomically (cmdPong client server serverName value) >>= sendMessage client
      (value:_) -> atomically (cmdPong client server serverName value) >>= sendMessage client
      _ -> atomically (errNeedMoreParams client server M.Ping) >>= sendMessage client

    return True

  M.LUsers -> handleLUsersMessage client server
  M.Motd -> handleMotdMessage client server
  M.Join -> handleJoinMessage client server params
  M.Part -> handlePartMessage client server params
  M.Names -> handleNamesMessage client server params

  M.UnknownCommand command -> do
    atomically (errUnknownCommand client server command) >>= sendMessage client
    return True

  c -> do
    atomically (errUnknownCommand client server (fromJust $ M.getCommandName c)) >>= sendMessage client
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
      case A.parseOnly P.message $ T.take M.maxMessageLength line of
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
    U.User {U.hostmask = U.Hostmask{U.nickname = nickname}, U.registered = registered, U.channels = channels} <- readTVar user

    when registered $ do
      modifyTVar clients $ \clients -> do
        Map.delete (U.normalizeNickname nickname) clients

    mapM_ (partChannel' client server) (Set.toList channels)

    return $ do
      debugM "Merc.Server" $ "Deleted user " ++ show nickname

  where
    handle = S.handle client

runServer :: S.Server -> PortID -> IO ()
runServer server port = withSocketsDo $ do
  sock <- listenOn port
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

newServer :: T.Text -> T.Text -> T.Text -> IO S.Server
newServer serverName networkName motd = do
  clients <- newTVarIO Map.empty
  channels <- newTVarIO Map.empty

  now <- getCurrentTime

  return S.Server {
    S.serverName = serverName,
    S.networkName = networkName,
    S.motd = motd,
    S.clients = clients,
    S.channels = channels,
    S.creationTime = now
  }

