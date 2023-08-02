module SupportFunctions where

import Data.Map.Strict
import OpenGames.Engine.Engine
import Types

{--
Contains basic auxiliary functionality needed for model
-}

-- Constants

bpsPower :: Int
bpsPower = 10000

-- State transition functions

-- | State transition for adding liquidity to the pool:
-- Calculate token price (for now it'll be hardcoded and EMA and regular price will be equal
-- Calculate fee based on EMA price
-- Calculate USD value of input tokens tokens_usd = (tokens_in - fee) * token_price
-- Calculate USD value of all assets under management in the pool
-- Calculate total supply of LP tokens
-- Mint n LP tokens where n = if aum_usd == 0 then tokens_usd else tokens_usd * lp_supply / aum_usd
-- Output new state:
--   user.assets.token -= tokens_in
--   user.assets.lp_token += n
--   pool.assets.token += tokens_in
--   pool.lp_supply += n
addLiquidity ::
  -- | Input state
  State ->
  -- | Account adding liquidity
  AccountName ->
  -- | Target LP
  PoolName ->
  -- | Asset being added
  AssetName ->
  -- | Amount of asset being added
  AssetQuantity ->
  -- | Slippage / Minimum amount of LP tokens expected
  AssetQuantity ->
  -- | Updated state
  State
addLiquidity state accountName poolName assetName amountIn slippage =
  let pool = pools state ! poolName
      account = accounts state ! accountName
      assetPrice = lookupOrZero assetName (priceIndex pool)
      fee = getFee pool assetName amountIn (addLiquidityFee $ fees pool)
      totalAssetValueUSD = (amountIn - fee) * assetPrice
      aumUSD = totalAUM pool
      lpTokensToMint =
        if aumUSD == 0
          then totalAssetValueUSD
          else totalAssetValueUSD * (outstandingSupply pool / aumUSD)
   in if lpTokensToMint < slippage || lookupOrZero assetName (assets account) < amountIn
        then state
        else
          let account' =
                Account
                  { lamports = lamports account,
                    assets = adjust (\x -> x - amountIn) assetName (insertWith (+) poolName lpTokensToMint (assets account)),
                    longPosition = longPosition account,
                    shortPosition = shortPosition account,
                    collateralizedAssets = collateralizedAssets account
                  }
              pool' =
                Pool
                  { outstandingSupply = lpTokensToMint + outstandingSupply pool,
                    lpAssets = insertWith (+) assetName amountIn (lpAssets pool),
                    priceIndex = priceIndex pool,
                    assetRatios = assetRatios pool,
                    fees = fees pool,
                    pricingParams = pricingParams pool
                  }
           in State
                { accounts = insert accountName account' (accounts state),
                  pools = insert poolName pool' (pools state),
                  externalPriceIndex = externalPriceIndex state
                }

-- | State transition for removing liquidity from the pool:
-- Calculate output token price
-- Calculate USD value of all assets under management in the pool
-- Calculate USD value of the output remove_amount_usd = aum_usd * lp_token_in / lp_supply
-- Calculate amount of output tokens remove_amount = remove_amount_usd / token_price
-- Calculate fee and amount to transfer transfer_amount = remove_amount - fee
-- Burn lp_token_in LP tokens
removeLiquidity ::
  -- | Input state
  State ->
  -- | Account removing liquidity
  AccountName ->
  -- | Target LP
  PoolName ->
  -- | Asset being removed
  AssetName ->
  -- | Amount of LP tokens being added
  AssetQuantity ->
  -- | Slippage / Minimum amount of assetName expected
  AssetQuantity ->
  -- | Updated state
  State
removeLiquidity state accountName poolName assetName lpTokenIn slippage =
  let pool = pools state ! poolName
      account = accounts state ! accountName
      assetPrice = lookupOrZero assetName (priceIndex pool)
      aumUSD = totalAUM pool
      removeAmountUSD = aumUSD * lpTokenIn / outstandingSupply pool
      fee = getFee pool assetName lpTokenIn (removeLiquidityFee $ fees pool)
      removeAmount = removeAmountUSD / assetPrice
      feeAmount = fee / assetPrice
      tokensOut = removeAmount - feeAmount
      ownedTokenAmountLP = lpAssets pool ! assetName
   in if tokensOut < slippage || ownedTokenAmountLP < tokensOut || lpTokenIn > lookupOrZero poolName (assets account)
        then state
        else
          let account' =
                Account
                  { lamports = lamports account,
                    assets = adjust (\x -> x - lpTokenIn) poolName (insertWith (+) assetName tokensOut (assets account)),
                    longPosition = longPosition account,
                    shortPosition = shortPosition account,
                    collateralizedAssets = collateralizedAssets account
                  }
              pool' =
                Pool
                  { outstandingSupply = outstandingSupply pool - lpTokenIn,
                    lpAssets = insertWith (-) assetName tokensOut (lpAssets pool),
                    priceIndex = priceIndex pool,
                    assetRatios = assetRatios pool,
                    fees = fees pool,
                    pricingParams = pricingParams pool
                  }
           in State
                { accounts = insert accountName account' (accounts state),
                  pools = insert poolName pool' (pools state),
                  externalPriceIndex = externalPriceIndex state
                }

-- | Token swap state transition
swap ::
  -- | Input state
  State ->
  -- | Account removing liquidity
  AccountName ->
  -- | Target LP
  PoolName ->
  -- | Input asset
  AssetName ->
  -- | Output asset
  AssetName ->
  -- | Amount of input asset
  AssetQuantity ->
  -- | Slippage / Minimum amount of output asset expected
  AssetQuantity ->
  -- | Updated state
  State
swap state accountName poolName inputAssetName outputAssetName amountIn slippage =
  let pool = pools state ! poolName
      account = accounts state ! accountName
      inputAssetPrice = lookupOrZero inputAssetName (priceIndex pool)
      outputAssetPrice = lookupOrZero outputAssetName (priceIndex pool)
      ownedOutAssetLP = lpAssets pool ! outputAssetName
      inputAssetUSD = amountIn * inputAssetPrice
      pairPrice = inputAssetPrice / outputAssetPrice
      swapPrice = getPrice pairPrice Short (swapSpread $ pricingParams pool)
      swapAmount = amountIn * swapPrice
      fee = getFee pool inputAssetName amountIn (swapFee $ fees pool)
      tokensOut = swapAmount - fee
   in if tokensOut < slippage || ownedOutAssetLP < tokensOut
        then state
        else
          let account' =
                Account
                  { lamports = lamports account,
                    assets = adjust (\x -> x - amountIn) inputAssetName (insertWith (+) outputAssetName tokensOut (assets account)),
                    longPosition = longPosition account,
                    shortPosition = shortPosition account,
                    collateralizedAssets = collateralizedAssets account
                  }
              pool' =
                Pool
                  { outstandingSupply = outstandingSupply pool,
                    lpAssets = adjust (\x -> x - tokensOut) outputAssetName (insertWith (+) inputAssetName amountIn (assets account)),
                    priceIndex = priceIndex pool,
                    assetRatios = assetRatios pool,
                    fees = fees pool,
                    pricingParams = pricingParams pool
                  }
           in State
                { accounts = insert accountName account' (accounts state),
                  pools = insert poolName pool' (pools state),
                  externalPriceIndex = externalPriceIndex state
                }

-- | Add collateral state transition
addCollateral ::
  State ->
  AccountName ->
  PoolName ->
  AssetName ->
  AssetQuantity ->
  State
addCollateral state accountName poolName assetName tokenAmount = undefined

-- | Remove collateral state transition
removeCollateral ::
  State ->
  AccountName ->
  PoolName ->
  AssetName ->
  AssetQuantity ->
  State
removeCollateral state accountName poolName assetName tokenAmount = undefined

-- | Open position state transition
openPosition ::
  State ->
  AccountName ->
  PoolName ->
  AssetName ->
  -- | Position price
  Price ->
  -- | Collateral
  Int ->
  -- | Position size
  Int ->
  Side ->
  State
openPosition state accountName poolName assetName price collateral size side = undefined

-- | Close position state transition
closePosition ::
  State ->
  AccountName ->
  PoolName ->
  AssetName ->
  -- | Position price
  Price ->
  State
closePosition state accountName poolName price = undefined

-- | Liquidate position state transition
liquidatePosition ::
  State ->
  AccountName ->
  PoolName ->
  AssetName ->
  State
liquidatePosition state accountName poolName price = undefined

-- General helper functions

-- | General function for calculating fees
getFee :: Pool -> AssetName -> AssetQuantity -> Double -> AssetQuantity
getFee pool assetName assetQuantity baseFee =
  let currentRatio = getCurrentRatio pool assetName
      tokenRatio = assetRatios pool ! assetName
      target = targetRatio tokenRatio
      newRatio = getNewRatio pool assetName assetQuantity
      ratioMult = ratioMultiplier $ fees pool
      improved = case compare newRatio target of
        LT ->
          newRatio > currentRatio
            || ( currentRatio > target
                   && currentRatio - target > target - newRatio
               )
        GT ->
          newRatio < currentRatio
            || ( currentRatio < target
                   && target - currentRatio > newRatio - target
               )
        EQ -> currentRatio /= target
      ratioFee =
        1 + ratioMult
            * ( if newRatio < target
                  then (target - newRatio) / (target - minRatio tokenRatio)
                  else (newRatio - target) / (maxRatio tokenRatio - target)
              )
      fee =
        if improved
          then baseFee / ratioFee
          else baseFee * ratioFee
   in (fee * assetQuantity) / fromIntegral bpsPower

-- | Helper: Lookup a given element in the pool, return 0 if not found.
-- We will assume that if Asset x is not listed in the pool, then quantity x = 0, same for price.
lookupOrZero :: (Ord k, Num a) => k -> Map k a -> a
lookupOrZero n m = case Data.Map.Strict.lookup n m of
  Just k -> k
  _ -> 0

-- | Calculate the USD value for a certain asset type under management in the pool
getTokenAUM :: Pool -> AssetName -> Price
getTokenAUM pool tokenName =
  let assetQuantity = lookupOrZero tokenName (lpAssets pool)
      assetPrice = lookupOrZero tokenName (priceIndex pool)
   in assetQuantity * assetPrice

-- | Calculate the current ratio of a token given its amount in the pool and the pool's total AUM
getCurrentRatio :: Pool -> AssetName -> Double
getCurrentRatio pool assetName =
  let aumUSD = totalAUM pool
      assetPrice = lookupOrZero assetName (priceIndex pool)
      heldAssetAmount = lookupOrZero assetName (lpAssets pool)
      totalAssetUSD = heldAssetAmount * assetPrice
   in (totalAssetUSD * fromIntegral bpsPower) / aumUSD

-- | Calculate the new ratio of a token after an operation
getNewRatio :: Pool -> AssetName -> AssetQuantity -> Double
getNewRatio pool assetName assetQuantity =
  let aumUSD = totalAUM pool
      assetPrice = lookupOrZero assetName (priceIndex pool)
      currentAssetAmount = lookupOrZero assetName (lpAssets pool)
      newAssetAmount = currentAssetAmount + assetQuantity
      assetInUSD = assetQuantity * assetPrice
      newAssetUSD = newAssetAmount * assetPrice
   in (currentAssetAmount * fromIntegral bpsPower) / (aumUSD + assetInUSD)

getPrice :: Price -> Side -> Double -> Price
getPrice assetPrice Long spread = assetPrice + (assetPrice * spread)
getPrice assetPrice Short spread =
  let newSpread = assetPrice * spread
   in if newSpread < assetPrice
        then assetPrice - newSpread
        else 0

-- | Calculate the USD value of total assets under management in the pool
totalAUM :: Pool -> Price
totalAUM pool = sum $ fmap (getTokenAUM pool) (keys $ lpAssets pool)

-- | Calculate the USD value of total assets under management for a specific account
accountPayoffUSD :: AccountName -> State -> Price
accountPayoffUSD accountName state =
  let account = accounts state ! accountName
      index = externalPriceIndex state
      assetNames = keys index
      assetQuantity assetName = lookupOrZero assetName (assets account)
      assetPrice assetName = lookupOrZero assetName (externalPriceIndex state)
   in sum $ fmap (\a -> assetQuantity a * assetPrice a) assetNames

-- | Compute difference between holdings; just a convenience function
differenceHoldings :: Price -> Price -> Price
differenceHoldings = (-)
