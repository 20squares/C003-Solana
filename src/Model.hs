{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module Model where

import ActionSpaces
import Components
import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import SupportFunctions
import Types

{-
Contains the basic state change logic for an individual user.
By instantiating a initial state, the incentives underlying each individual for any scenario can be checked.
-}


-- Example game where we add an remove liquidity (and obviously lose money in between)
-- TODO add payoffs function
exampleAddRemoveLiquidity name actionSpaceAdd actionSpaceRemove stateUpdateMechanism1 stateUpdateMechanism2 payoffsFromState exogenousNumeraireT1 exogenousNumeraireT2 =
  [opengame|

    inputs    :  state ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state ;
    feedback  :   ;
    operation :  addLiquidityGame name actionSpaceAdd ;
    outputs   :  addedLiquidity ;
    returns   :  paymentsAdd ;

    inputs    :  state, addedLiquidity ;
    feedback  :   ;
    operation :  updateStateGame name stateUpdateMechanism1 ;
    outputs   :  state2 ;
    returns   :   ;

    inputs    :  state, state2 ;
    feedback  :   ;
    operation :  forwardFunction $ payoffsFromState exogenousNumeraireT1 ;
    outputs   :  paymentsAdd ;
    returns   :   ;

    inputs    :  state2 ;
    feedback  :   ;
    operation :  removeLiquidityGame name actionSpaceRemove ;
    outputs   :  removedLiquidity ;
    returns   :  paymentsRemove ;

    inputs    :  state2, removedLiquidity ;
    feedback  :   ;
    operation :  updateStateGame name stateUpdateMechanism2 ;
    outputs   :  state3 ;
    returns   :   ;

    inputs    :  state2, state3 ;
    feedback  :   ;
    operation :  forwardFunction $ payoffsFromState exogenousNumeraireT2 ;
    outputs   :  paymentsRemove ;
    returns   :   ;

    :---------------------------:

    outputs   :  state3, paymentsAdd,paymentsRemove ;
    returns   :   ;
  |]

