module Merc.Types.Channel (
  ChannelName(..),
  NormalizedChannelName(..),
  ChannelUserFlags(..),
  Channel(..)
) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Merc.Types.User as U
import Merc.Util

newtype ChannelName = ChannelName {
  unwrapName :: T.Text
} deriving (Show)

newtype NormalizedChannelName = NormalizedChannelName {
  unwrapNormalizedName :: T.Text
} deriving (Eq, Ord, Show)

data ChannelUserFlags = ChannelUserFlags {
  op :: Bool,
  voice :: Bool
}

data Channel = Channel {
  name :: ChannelName,
  users :: M.Map U.Nickname ChannelUserFlags,
  private :: Bool
}

normalizeChannelName :: ChannelName -> NormalizedChannelName
normalizeChannelName ChannelName { unwrapName = unwrapped } =
  NormalizedChannelName (toIRCLower unwrapped)
