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

-- | Strategy tuple
strategyTuple addParameter removeParameter =
  addLiquidityStrategy addParameter
  ::- removeLiquidityStrategy removeParameter
  ::- Nil
