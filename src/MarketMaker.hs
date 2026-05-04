module MarketMaker (version) where

import Data.Version (showVersion)
import Paths_hs_market_maker qualified as Paths

version :: String
version = showVersion Paths.version
