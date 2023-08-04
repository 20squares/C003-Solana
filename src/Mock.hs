module Mock where

import Data.Map.Strict
import Types
import SupportFunctions

-- TEST IMPLEMENTATION
aki :: Account
aki =
  Account
    { lamports = 10,
      assets = fromList [("Sol", 10000000)],
      longPosition = empty,
      shortPosition = empty,
      collateralizedAssets = empty
    }

bogdan :: Account
bogdan =
  Account
    { lamports = 10,
      assets = fromList [("Doge", 100000000)],
      longPosition = empty,
      shortPosition = empty,
      collateralizedAssets = empty
    }

candice :: Account
candice =
  Account
    { lamports = 10,
      assets = fromList [("Doge", 100)],
      longPosition = empty,
      shortPosition = empty,
      collateralizedAssets = empty
    }

testFees :: Fees
testFees =
  Fees
    { openPositionFee = 1,
      closePositionFee = 1,
      swapFee = 1,
      liquidationFee = 1,
      addLiquidityFee = 1,
      removeLiquidityFee = 1,
      ratioMultiplier = 1
    }


testFees2 :: Fees
testFees2 =
  Fees
    { openPositionFee = 0,
      closePositionFee = 0,
      swapFee = 0,
      liquidationFee = 0,
      addLiquidityFee = 0,
      removeLiquidityFee = 0,
      ratioMultiplier = 0
    }

testPricing :: PricingParams
testPricing =
  PricingParams
    { swapSpread = 0
    }

testRatio :: Ratios
testRatio =
  Ratios
    { targetRatio = 4,
      minRatio = 1,
      maxRatio = 5
    }

test :: Pool
test =
  Pool
    { outstandingSupply = 100000000,
      lpAssets = fromList [("Doge", 10000), ("Sol", 100)],
      priceIndex = fromList [("Doge", 1), ("Sol", 10)],
      fees = testFees,
      assetRatios = fromList [("Doge", testRatio), ("Sol", testRatio)],
      pricingParams = testPricing
    }

test2 :: Pool
test2 =
  Pool
    { outstandingSupply = 0,
      lpAssets = empty,
      priceIndex = fromList [("Doge", 1), ("Sol", 10)],
      fees = testFees,
      assetRatios = fromList [("Doge", testRatio), ("Sol", testRatio)],
      pricingParams = testPricing
    }

test3 :: Pool
test3 =
  Pool
    { outstandingSupply = 10000000000,
      lpAssets = fromList [("Doge", 100000000000), ("Sol", 100000000)],
      priceIndex = fromList [("Doge", 1), ("Sol", 10)],
      fees = testFees2,
      assetRatios = fromList [("Doge", testRatio), ("Sol", testRatio)],
      pricingParams = testPricing
    }

state1 :: State
state1 =
  State
    { accounts = fromList [("Aki", aki), ("Bogdan", bogdan), ("Candice", candice)],
      pools = fromList [("Test", test), ("Test2", test2)],
      externalPriceIndex = fromList [("Doge", 1), ("Sol", 10)]

    }

state2 :: State
state2 =
  State
    { accounts = fromList [("Aki", aki)],
      pools = fromList [("Test2", test2)],
      externalPriceIndex = fromList [("Doge", 1), ("Sol", 10)]
    }

state3 :: State
state3 =
  State
    { accounts = fromList [("Aki", aki), ("Bogdan", bogdan)],
      pools = fromList [("Pool1", test3), ("Pool2", test3)],
      externalPriceIndex = fromList [("Doge", 1), ("Sol", 10)]
    }

swapTest = swapAssets "Aki" (state1, ("Test", "Sol", "Doge", 10, 0))

arbTest = let
  step1 = swapAssets "Aki" (state3, ("Pool1", "Sol", "Doge", 10000000, 0))
  step2 = swapAssets "Bogdan" (step1, ("Pool1", "Doge", "Sol", 100000000, 0))
  step3 = swapAssets "Bogdan" (step2, ("Pool2", "Sol", "Doge", 10000000, 0))
  in step3

arbTest2 = let
  step1 = swapAssets "Aki" (state3, ("Pool1", "Sol", "Doge", 10000000, 0))
  step2 = swapAssets "Bogdan" (step1, ("Pool2", "Sol", "Doge", 10000000, 0))
  step3 = swapAssets "Bogdan" (step2, ("Pool2", "Sol", "Doge", 10000000, 0))
  in step2
