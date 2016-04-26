module Util where

import Data.List

prefixLines :: String -> String -> String
prefixLines pfx = unlines . map (pfx++) . lines