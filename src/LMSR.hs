{- | Hanson 2002 logarithmic market scoring rule

https://mason.gmu.edu/~rhanson/mktscore.pdf
-}
module LMSR (
    -- * Market parameters
    Market (..),
    mkMarket,

    -- * Market state
    MarketState,
    initialState,
    applyTrade,
    marketStateVector,

    -- * Trades
    Trade,
    mkTrade,
    buyShares,
    tradeVector,

    -- * Pricing
    cost,
    prices,
    tradeCost,
    boundedLoss,

    -- * Errors
    MarketError (..),
) where

import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V

data Market = Market
    { outcomes :: !Int
    , liquidity :: !Double
    }
    deriving stock (Eq, Show)

mkMarket :: Int -> Double -> Either MarketError Market
mkMarket n b
    | n < 2 = Left (TooFewOutcomes n)
    | b <= 0 = Left (NonPositiveLiquidity b)
    | otherwise = Right Market{outcomes = n, liquidity = b}

newtype MarketState = MarketState (Vector Double)
    deriving stock (Eq, Show)

initialState :: Market -> MarketState
initialState params = MarketState (V.replicate params.outcomes 0)

applyTrade :: MarketState -> Trade -> MarketState
applyTrade (MarketState q) (Trade d) = MarketState (V.zipWith (+) q d)

marketStateVector :: MarketState -> Vector Double
marketStateVector (MarketState v) = v

-- Per-outcome share delta. Positive entries buy, negative entries sell.
newtype Trade = Trade (Vector Double)
    deriving stock (Eq, Show)

mkTrade :: Market -> Vector Double -> Either MarketError Trade
mkTrade params v
    | V.length v /= params.outcomes =
        Left (TradeShapeMismatch params.outcomes (V.length v))
    | otherwise = Right (Trade v)

buyShares :: Market -> Int -> Double -> Either MarketError Trade
buyShares params i shares
    | i < 0 || i >= params.outcomes = Left (OutcomeOutOfRange i)
    | shares <= 0 = Left (NonPositiveShares shares)
    | otherwise = Right (Trade (V.generate params.outcomes oneHot))
  where
    oneHot j = if j == i then shares else 0

tradeVector :: Trade -> Vector Double
tradeVector (Trade v) = v

cost :: Market -> MarketState -> Double
cost params state =
    let b = params.liquidity
        ys = V.map (/ b) (marketStateVector state)
     in b * logSumExp ys

prices :: Market -> MarketState -> Vector Double
prices params state =
    let b = params.liquidity
        ys = V.map (/ b) (marketStateVector state)
        m = V.maximum ys
        es = V.map (\y -> exp (y - m)) ys
        s = V.sum es
     in V.map (/ s) es

tradeCost :: Market -> MarketState -> Trade -> Double
tradeCost params state trade =
    let b = params.liquidity
        q = marketStateVector state
        d = tradeVector trade
        ys = V.map (/ b) q
        ys' = V.map (/ b) (V.zipWith (+) q d)
        m = max (V.maximum ys) (V.maximum ys')
        lse vs = m + log (V.sum (V.map (\v -> exp (v - m)) vs))
     in b * (lse ys' - lse ys)

-- Theoretical maximum loss the market maker can suffer regardless of trading volume
boundedLoss :: Market -> Double
boundedLoss params = params.liquidity * log (fromIntegral (params.outcomes :: Int))

data MarketError
    = TooFewOutcomes !Int
    | NonPositiveLiquidity !Double
    | TradeShapeMismatch !Int !Int
    | OutcomeOutOfRange !Int
    | NonPositiveShares !Double
    deriving stock (Eq, Show)

logSumExp :: Vector Double -> Double
logSumExp xs =
    let m = V.maximum xs
     in m + log (V.sum (V.map (\x -> exp (x - m)) xs))
