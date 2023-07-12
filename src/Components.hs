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
addLiquidity name actionSpace =
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
removeLiquidity name actionSpace =
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
swap name actionSpace =
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
openPosition name actionSpace =
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
closePosition name actionSpace =
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
addCollateral name actionSpace =
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
removeCollateral name actionSpace =
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
liquidatePosition name actionSpace =
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
updateState name stateUpdateMechanism =
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
