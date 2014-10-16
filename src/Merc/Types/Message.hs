module Merc.Types.Message (
  Prefix(..),
  Command(..),
  getCommandName,
  fromCommandName,
  Message(..)
) where

import qualified Data.Bimap as B
import qualified Data.Text as T
import qualified Merc.Types.User as U

data Prefix = HostmaskPrefix U.Hostmask
            | ServerPrefix T.Text
            deriving (Show)

data Command = RplWelcome
             | RplYourHost
             | RplCreated
             | RplMyInfo
             | ErrNeedMoreParams
             | Nick
             | User
             | Join
             | Names
             | Part
             | Ping
             | Pong
             | Privmsg
             | Quit
             | UnknownCommand T.Text
             deriving (Eq, Ord, Show)

commandNames :: B.Bimap Command T.Text
commandNames = B.fromList [
  (RplWelcome, "001"),
  (RplYourHost, "002"),
  (RplCreated, "003"),
  (RplMyInfo, "004"),
  (ErrNeedMoreParams, "461"),
  (Nick, "NICK"),
  (User, "USER"),
  (Join, "JOIN"),
  (Part, "PART"),
  (Ping, "PING"),
  (Pong, "PONG"),
  (Names, "NAMES"),
  (Privmsg, "PRIVMSG"),
  (Quit, "QUIT")]

getCommandName :: Command -> Maybe T.Text
getCommandName command = B.lookup command commandNames

fromCommandName :: T.Text -> Maybe Command
fromCommandName commandName = B.lookupR commandName commandNames

data Message = Message {
  prefix :: Maybe Prefix,
  command :: Command,
  params :: [T.Text]
} deriving (Show)
