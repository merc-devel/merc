module Merc.Emitter (
  emitHostmask,
  emitMessage
) where

import Control.Monad
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Merc.Types.Message as M
import qualified Merc.Types.User as U

emitHostmask :: U.Hostmask -> T.Text
emitHostmask U.Hostmask{..} =
  U.unwrapName nickname <> "!" <> username <> "@" <> host

emitPrefix :: M.Prefix -> T.Text
emitPrefix prefix = ":" <> case prefix of
  M.ServerPrefix serverName -> serverName
  M.HostmaskPrefix hostmask -> emitHostmask hostmask

emitParams :: [T.Text] -> T.Text
emitParams [] = ""
emitParams params | T.any (== ' ') trailing = T.intercalate " " initial <> " :" <> trailing
                  | otherwise = T.intercalate " " params
  where
    initial = fromJust $ do
      let leading = init params

      guard $ any (T.all (/= ' ')) leading
      guard $ all (/="") leading

      return leading
    trailing = last params

emitMessage :: M.Message -> T.Text
emitMessage M.Message{..} =
  front <> commandName <> back
  where
    front = maybe "" ((<> " ") . emitPrefix) prefix
    back = if params == [] then "" else " " <> emitParams params
    commandName = fromJust $ M.getCommandName command
