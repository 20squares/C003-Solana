{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Components where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import SupportFunctions

{-
Describes the basic available actions for actors in the market
-}

------------------
-- Pool operations

-- Add liquidity
addLiquidityGame name actionSpace =
  [opengame|

    inputs    :  state ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  addedLiquidity ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  addedLiquidity ;
    returns   :  payments ;
  |]

-- Remove liquidity
removeLiquidityGame name actionSpace =
  [opengame|

    inputs    :  state ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  removedLiquidity ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  removedLiquidity ;
    returns   :  payments ;
  |]

-- Remove liquidity
swapGame name actionSpace =
  [opengame|

    inputs    :  state ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  swapped ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  swapped ;
    returns   :  payments ;
  |]

---------------------
-- Trading operations

-- Open position
openPositionGame name actionSpace =
  [opengame|

    inputs    :  state ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  openedPosition ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  openedPosition ;
    returns   :  payments ;
  |]

-- Close position
closePositionGame name actionSpace =
  [opengame|

    inputs    :  state ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  closedPosition ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  closedPosition ;
    returns   :  payments ;
  |]

-- Add collateral
addCollateralGame name actionSpace =
  [opengame|

    inputs    :  state ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  addedCollateral ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  addedCollateral ;
    returns   :  payments ;
  |]

-- Remove collateral
removeCollateralGame name actionSpace =
  [opengame|

    inputs    :  state ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  removedCollateral ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  removedCollateral ;
    returns   :  payments ;
  |]

-- Liquidate position
liquidatePositionGame name actionSpace =
  [opengame|

    inputs    :  state ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  liquidatedPosition ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  liquidatedPosition ;
    returns   :  payments ;
  |]

---------------------------
-- General state operations

-- Update state
updateStateGame name stateUpdateMechanism =
  [opengame|

    inputs    :  state, action ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, action ;
    feedback  :   ;
    operation :  forwardFunction stateUpdateMechanism ;
    outputs   :  newState ;
    returns   :   ;

    :---------------------------:

    outputs   :  newState ;
    returns   :   ;
  |]
