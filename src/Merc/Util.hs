module Merc.Util (
    toIRCLower
) where

import qualified Data.Text as T

toIRCLower :: T.Text -> T.Text
toIRCLower = T.toLower
