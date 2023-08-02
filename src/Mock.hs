module Mock where

import Data.Map.Strict
import Types

-- TEST IMPLEMENTATION
aki :: Account
aki =
  Account
    { lamports = 10,
      assets = fromList [("Sol", 100)],
      longPosition = empty,
      shortPosition = empty,
      collateralizedAssets = empty
    }

bogdan :: Account
bogdan =
  Account
    { lamports = 10,
      assets = fromList [("Doge", 100)],
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
