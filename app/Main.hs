module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector.Unboxed qualified as V
import Data.Version (showVersion)
import Paths_lmsr qualified as Paths

import LMSR qualified as Core

main :: IO ()
main = do
    TIO.putStrLn $ "lmsr " <> T.pack (showVersion Paths.version)
    runServer

runServer :: IO ()
runServer = do
    TIO.putStrLn "[engine] starting (stub)"
    case Core.mkMarket 2 100 of
        Right params -> do
            let ps = Core.prices params (Core.initialState params)
            TIO.putStrLn $
                "[engine] sanity: 2-outcome market initialized, prices = "
                    <> T.pack (show (V.toList ps))
        Left e ->
            TIO.putStrLn $ "[engine] failed to initialize default market: " <> T.pack (show e)
    TIO.putStrLn "[engine] no transport bound (HTTP/gRPC TBD); exiting"
