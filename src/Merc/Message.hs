module Merc.Message (
  newServerMessage,
  sendMessage,
  needMoreParams
) where

import Control.Concurrent.STM
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Merc.Emitter
import qualified Merc.Types.Message as M
import qualified Merc.Types.Server as S
import qualified Merc.Types.User as U
import Network

newServerMessage :: S.Server -> M.Command -> [T.Text] -> M.Message
newServerMessage S.Server{..} command params = M.Message {
  M.prefix = Just $ M.ServerPrefix serverName,
  M.command = command,
  M.params = params
}
  where
    prefix = Just $ M.ServerPrefix serverName

sendMessage :: S.Client -> M.Message -> IO ()
sendMessage S.Client{..} message =
  T.hPutStrLn handle $ emitMessage message

needMoreParams :: S.Client -> S.Server -> M.Command -> STM M.Message
needMoreParams S.Client{..} server command = do
  U.User{U.hostmask = U.Hostmask{U.nickname = U.Nickname nickname}} <- readTVar user
  return $ newServerMessage server M.ErrNeedMoreParams [nickname, commandName, "Not enough parameters"]
  where
    commandName = fromJust $ M.getCommandName command
