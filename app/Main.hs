module Main where

import MarketMaker (version)

main :: IO ()
main = putStrLn $ "hs-market-maker " <> version
