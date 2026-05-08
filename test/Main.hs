module Main (main) where

import Data.Foldable (for_)
import Data.List (foldl')
import Data.Vector.Unboxed qualified as V
import Hedgehog (Gen, Property, assert, diff, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import LMSR
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "lmsr"
        [ propsLMSR
        , examplesLMSR
        ]

propsLMSR :: TestTree
propsLMSR =
    testGroup
        "LMSR properties"
        [ testProperty "initial prices are uniform" prop_initialUniform
        , testProperty "prices sum to 1" prop_pricesSumToOne
        , testProperty "prices are positive" prop_pricesPositive
        , testProperty "tradeCost equals C(q+δ) - C(q)" prop_tradeCostEqDeltaC
        , testProperty "path independent: state" prop_pathIndependentState
        , testProperty "path independent: cost" prop_pathIndependentCost
        , testProperty "zero trade has zero cost" prop_zeroTradeFree
        , testProperty "buying outcome i raises p_i" prop_buyingRaisesPrice
        , testProperty "prices are shift-invariant" prop_pricesShiftInvariant
        ]

examplesLMSR :: TestTree
examplesLMSR =
    testGroup
        "LMSR worked examples"
        [ testCase "Hanson b=100, 2 outcomes, buy 50 of outcome 0" hanson_2outcome
        ]

prop_initialUniform :: Property
prop_initialUniform = property $ do
    params <- forAll genMarketParams
    let ps = V.toList (prices params (initialState params))
        target = 1.0 / fromIntegral params.outcomes
    for_ ps $ \p -> diff p nearly target

prop_pricesSumToOne :: Property
prop_pricesSumToOne = property $ do
    params <- forAll genMarketParams
    s <- forAll (genState params)
    diff (V.sum (prices params s)) nearly 1.0

prop_pricesPositive :: Property
prop_pricesPositive = property $ do
    params <- forAll genMarketParams
    s <- forAll (genState params)
    for_ (V.toList (prices params s)) $ \p -> assert (p > 0)

prop_tradeCostEqDeltaC :: Property
prop_tradeCostEqDeltaC = property $ do
    params <- forAll genMarketParams
    s <- forAll (genState params)
    t <- forAll (genTrade params)
    let computed = tradeCost params s t
        delta = cost params (applyTrade s t) - cost params s
    diff computed nearly delta

prop_pathIndependentState :: Property
prop_pathIndependentState = property $ do
    params <- forAll genMarketParams
    s <- forAll (genState params)
    t1 <- forAll (genTrade params)
    t2 <- forAll (genTrade params)
    let v12 = V.toList (marketStateVector (applyTrade (applyTrade s t1) t2))
        v21 = V.toList (marketStateVector (applyTrade (applyTrade s t2) t1))
    for_ (zip v12 v21) $ \(a, b) -> diff a nearly b

prop_pathIndependentCost :: Property
prop_pathIndependentCost = property $ do
    params <- forAll genMarketParams
    s <- forAll (genState params)
    t1 <- forAll (genTrade params)
    t2 <- forAll (genTrade params)
    let viaT1 = tradeCost params s t1 + tradeCost params (applyTrade s t1) t2
        viaT2 = tradeCost params s t2 + tradeCost params (applyTrade s t2) t1
    diff viaT1 nearly viaT2

prop_zeroTradeFree :: Property
prop_zeroTradeFree = property $ do
    params <- forAll genMarketParams
    s <- forAll (genState params)
    let zero = unsafeRight (mkTrade params (V.replicate params.outcomes 0))
    diff (tradeCost params s zero) nearly 0.0

prop_buyingRaisesPrice :: Property
prop_buyingRaisesPrice = property $ do
    params <- forAll genMarketParams
    s <- forAll (genState params)
    i <- forAll (Gen.int (Range.linear 0 (params.outcomes - 1)))
    shares <- forAll (Gen.double (Range.linearFrac 0.001 100.0))
    let t = unsafeRight (buyShares params i shares)
        pBefore = prices params s V.! i
        pAfter = prices params (applyTrade s t) V.! i
    assert (pAfter >= pBefore)

prop_pricesShiftInvariant :: Property
prop_pricesShiftInvariant = property $ do
    params <- forAll genMarketParams
    s <- forAll (genState params)
    c <- forAll (Gen.double (Range.linearFrac (-50) 50))
    let shift = unsafeRight (mkTrade params (V.replicate params.outcomes c))
        ps = V.toList (prices params s)
        psShift = V.toList (prices params (applyTrade s shift))
    for_ (zip ps psShift) $ \(a, b) -> diff a nearly b

hanson_2outcome :: IO ()
hanson_2outcome = do
    let params = unsafeRight (mkMarketParams 2 100)
        s0 = initialState params
        buy50 = unsafeRight (buyShares params 0 50)
        s1 = applyTrade s0 buy50
    shouldBeNear "C(0,0)" (cost params s0) (100 * log 2)
    shouldBeNear "C(50,0)" (cost params s1) (100 * log (exp 0.5 + 1))
    shouldBeNear "trade cost" (tradeCost params s0 buy50) (100 * (log (exp 0.5 + 1) - log 2))
    shouldBeNear "boundedLoss" (boundedLoss params) (100 * log 2)
    let ps = V.toList (prices params s1)
        pYes = exp 0.5 / (exp 0.5 + 1)
    shouldBeNear "p(Yes) after" (head ps) pYes
    shouldBeNear "p(No)  after" (ps !! 1) (1 - pYes)

genMarketParams :: Gen MarketParams
genMarketParams = do
    n <- Gen.int (Range.linear 2 8)
    b <- Gen.double (Range.linearFrac 1.0 1000.0)
    pure (unsafeRight (mkMarketParams n b))

genState :: MarketParams -> Gen MarketState
genState params = do
    ts <- Gen.list (Range.linear 0 5) (genTrade params)
    pure (foldl' applyTrade (initialState params) ts)

genTrade :: MarketParams -> Gen Trade
genTrade params = do
    ds <- Gen.list (Range.singleton params.outcomes) (Gen.double (Range.linearFrac (-50) 50))
    pure (unsafeRight (mkTrade params (V.fromList ds)))

nearly :: Double -> Double -> Bool
nearly a b = abs (a - b) <= eps + eps * max (abs a) (abs b)
  where
    eps = 1e-8

shouldBeNear :: String -> Double -> Double -> IO ()
shouldBeNear label actual expected =
    assertBool
        (label <> ": " <> show actual <> " ≉ " <> show expected)
        (nearly actual expected)

unsafeRight :: (Show e) => Either e a -> a
unsafeRight = either (error . show) id
