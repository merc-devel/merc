module Merc.Types.Channel (
  Role(..),
  ChannelName(..),
  NormalizedChannelName(..),
  ChannelUser(..),
  Channel(..),
  UserPrefix(..),
  ChannelMode(..),
  normalizeChannelName,
  channelModesWithParams,
  channelModes,
  rolePrefixes,
  roleModes,
  showChannelName
) where

import qualified Data.Bimap as B
import qualified Data.Map as M
import Data.List
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Merc.Types.User as U
import Merc.Util

newtype ChannelName = ChannelName {
  unwrapName :: T.Text
} deriving (Show)

newtype NormalizedChannelName = NormalizedChannelName {
  unwrapNormalizedName :: T.Text
} deriving (Eq, Ord, Show)

data Role = Owner
          | Administrator
          | Operator
          | HalfOperator
          | Voiced
          | NoRole
          deriving (Eq, Ord, Show)

newtype UserPrefix = UserPrefix {
  unwrapUserPrefix :: Char
} deriving (Eq, Ord, Show)

newtype ChannelMode = ChannelMode {
  unwrapChannelMode :: Char
} deriving (Eq, Ord, Show)

rolePrefixesMap :: B.Bimap Role UserPrefix
rolePrefixesMap = B.fromList [
  (Owner, UserPrefix '~'),
  (Administrator, UserPrefix '&'),
  (Operator, UserPrefix '@'),
  (HalfOperator, UserPrefix '%'),
  (Voiced, UserPrefix '+')]

roleModesMap :: B.Bimap Role ChannelMode
roleModesMap = B.fromList [
  (Owner, ChannelMode 'q'),
  (Administrator, ChannelMode 'a'),
  (Operator, ChannelMode 'o'),
  (HalfOperator, ChannelMode 'h'),
  (Voiced, ChannelMode 'v')]

rolePrefixes :: [UserPrefix]
rolePrefixes = map snd $ B.assocs rolePrefixesMap

roleModes :: [ChannelMode]
roleModes = map snd $ B.assocs roleModesMap

channelModes :: S.Set ChannelMode
channelModes = S.fromList $ [ChannelMode 'p']

channelModesWithParams :: S.Set ChannelMode
channelModesWithParams = S.fromList roleModes

data ChannelUser = ChannelUser {
  role :: Role
} deriving (Show)

data Channel = Channel {
  name :: ChannelName,
  users :: M.Map U.NormalizedNickname ChannelUser,
  topic :: T.Text
}

normalizeChannelName :: ChannelName -> NormalizedChannelName
normalizeChannelName ChannelName { unwrapName = unwrapped } =
  NormalizedChannelName (toIRCLower unwrapped)

showChannelName :: ChannelName -> T.Text
showChannelName ChannelName { unwrapName = name } = "#" <> name
