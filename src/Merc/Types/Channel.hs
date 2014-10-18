module Merc.Types.Channel (
  ChannelName(..),
  NormalizedChannelName(..),
  ChannelUser(..),
  Channel(..),
  channelModesWithParams,
  channelModes
) where

import qualified Data.Bimap as B
import qualified Data.Map as M
import Data.List
import qualified Data.Text as T
import qualified Merc.Types.User as U
import Merc.Util

newtype ChannelName = ChannelName {
  unwrapName :: T.Text
} deriving (Show)

newtype NormalizedChannelName = NormalizedChannelName {
  unwrapNormalizedName :: T.Text
} deriving (Eq, Ord, Show)

data Role = Operator
          | Voiced
          deriving (Eq, Ord, Show)

newtype UserPrefix = UserPrefix {
  unwrapUserPrefix :: Char
} deriving (Eq, Ord, Show)

newtype ChannelMode = ChannelMode {
  unwrapChannelMode :: Char
} deriving (Eq, Ord, Show)

roleChars :: B.Bimap Role UserPrefix
roleChars = B.fromList [
  (Operator, UserPrefix '@'),
  (Voiced, UserPrefix '+')]

roleModes :: B.Bimap Role ChannelMode
roleModes = B.fromList [
  (Operator, ChannelMode 'o'),
  (Voiced, ChannelMode 'v')]

channelModes :: [Char]
channelModes = "p"

channelModesWithParams :: [Char]
channelModesWithParams = map unwrapChannelMode $ sort $ B.keysR roleModes

data ChannelUser = ChannelUser {
  role :: Role
} deriving (Show)

data Channel = Channel {
  name :: ChannelName,
  users :: M.Map U.NormalizedNickname ChannelName,
  topic :: T.Text
}

normalizeChannelName :: ChannelName -> NormalizedChannelName
normalizeChannelName ChannelName { unwrapName = unwrapped } =
  NormalizedChannelName (toIRCLower unwrapped)
