{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}


module Components
  where

import SupportFunctions

import OpenGames.Engine.Engine
import OpenGames.Preprocessor


{-
Describes the basic available actions for actors in the market
-}

-- Open position
openPosition name actionSpace = [opengame|

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
closePosition name actionSpace = [opengame|

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


-- Add liquidity
addLiquidity name actionSpace = [opengame|

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
removeLiquidity name actionSpace = [opengame|

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


-- Update state
stateUpdateFunction name stateUpdateMechanism = [opengame|

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

