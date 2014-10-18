module Merc.Util (
    toIRCLower,
    mercVersion
) where

import Data.Monoid
import qualified Data.Text as T
import Data.Version
import Paths_merc (version)

toIRCLower :: T.Text -> T.Text
toIRCLower = T.toLower

mercVersion :: T.Text
mercVersion = "merc-" <> T.pack (showVersion version)
