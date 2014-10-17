module Merc.Types.User (
  Nickname(..),
  NormalizedNickname(unwrapNormalizedName),
  Hostmask(..),
  User(..),
  normalizeNickname,
  showNickname
) where

import qualified Data.Text as T
import Data.Time.Clock
import Merc.Util
import Network

data Nickname = UnregisteredNickname | Nickname {
  unwrapName :: T.Text
} deriving (Show)

newtype NormalizedNickname = NormalizedNickname {
  unwrapNormalizedName :: T.Text
} deriving (Eq, Ord, Show)

data Hostmask = Hostmask {
  nickname :: Nickname,
  username :: T.Text,
  host :: T.Text
} deriving (Show)

data User = User {
  hostmask :: Hostmask,
  realname :: T.Text,
  realHost :: HostName,
  connectionTime :: UTCTime,
  lastActiveTime :: UTCTime,
  registered :: Bool
}

normalizeNickname :: Nickname -> NormalizedNickname
normalizeNickname Nickname { unwrapName = unwrapped } =
  NormalizedNickname (toIRCLower unwrapped)

showNickname :: Nickname -> T.Text
showNickname UnregisteredNickname = "*"
showNickname Nickname { unwrapName = nickname } = nickname
