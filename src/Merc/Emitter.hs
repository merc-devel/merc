module Merc.Emitter (
  emitHostmask,
  emitMessage
) where

import qualified Data.Bimap as B
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Merc.Types.Message as M
import qualified Merc.Types.User as U

emitHostmask :: U.Hostmask -> T.Text
emitHostmask U.Hostmask{U.nickname = maybeNickname, U.username = maybeUsername, U.host = host} =
  nickname <> "!" <> username <> "@" <> host

  where
    U.Nickname nickname = maybe (U.Nickname "") id maybeNickname
    username = maybe "" id maybeUsername

emitPrefix :: M.Prefix -> T.Text
emitPrefix prefix = ":" <> case prefix of
  M.ServerPrefix serverName -> serverName
  M.HostmaskPrefix hostmask -> emitHostmask hostmask

emitParams :: [T.Text] -> T.Text
emitParams [] = ""
emitParams params = T.intercalate " " initial <> " :" <> trailing
  where
    initial = init params
    trailing = last params

emitMessage :: M.Message -> T.Text
emitMessage M.Message{M.prefix = maybePrefix, M.command = command, M.params = params} =
  front <> commandName <> back
  where
    front = maybe "" ((<> " ") . emitPrefix) maybePrefix
    back = if params == [] then "" else " " <> emitParams params
    commandName = fromJust $ B.lookup command M.commandNames
