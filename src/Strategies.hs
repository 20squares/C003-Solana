{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Strategies where

import OpenGames.Engine.Engine
import Payoffs
import SupportFunctions
import Types

{-
Defines the strategies
-}


-- | Added a parameterized asset
addLiquidityStrategy ::
  (PoolName, AssetName, AssetQuantity, AssetQuantity) ->
  Kleisli
     Stochastic
     State
     (PoolName, AssetName, AssetQuantity, AssetQuantity)
addLiquidityStrategy parameter =
  pureAction parameter

-- | Remove a parameterized asset
removeLiquidityStrategy ::
  (PoolName, AssetName, AssetQuantity, AssetQuantity) ->
  Kleisli
     Stochastic
     State
     (PoolName, AssetName, AssetQuantity, AssetQuantity)
removeLiquidityStrategy parameter =
  pureAction parameter

-- | Do a swap
swapStrategy ::
  (PoolName, AssetName, AssetName, AssetQuantity, AssetQuantity) ->
  Kleisli
     Stochastic
     State
     (PoolName, AssetName, AssetName, AssetQuantity, AssetQuantity)
swapStrategy parameter =
  pureAction parameter




-- | Strategy tuple for adding/removing liquidity
strategyTupleAddRemoveLiquidity addParameter removeParameter =
  addLiquidityStrategy addParameter
  ::- removeLiquidityStrategy removeParameter
  ::- Nil

-- | Strategy tuple for swaps
strategyTupleSwap swap1Par swap2Par swap3Par =
  swapStrategy swap1Par
  ::- swapStrategy swap2Par
  ::- swapStrategy swap3Par
  ::- Nil
