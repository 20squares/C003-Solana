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
-- NOTE: We assume that we keep the prices intact between different states
exampleAddRemoveLiquidity name actionSpaceAdd actionSpaceRemove =
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
    operation :  forwardFunction $ addLiquidity name ;
    outputs   :  state2 ;
    returns   :  ;

    inputs    :  state, state2 ;
    feedback  :   ;
    operation :  computePayments name ;
    outputs   :  paymentsAdd ;
    returns   :   ;

    inputs    :  state2 ;
    feedback  :   ;
    operation :  removeLiquidityGame name actionSpaceRemove ;
    outputs   :  removedLiquidity ;
    returns   :  paymentsRemove ;

    inputs    :  state2, removedLiquidity ;
    feedback  :   ;
    operation :  forwardFunction $ removeLiquidity name ;
    outputs   :  state3 ;
    returns   :  ;

    inputs    :  state2, state3 ;
    feedback  :   ;
    operation :  computePayments name ;
    outputs   :  paymentsRemove ;
    returns   :   ;

    :---------------------------:

    outputs   :  state3, paymentsAdd, paymentsRemove ;
    returns   :   ;
  |]

-- Include an additional private payoff from the tx
exampleAddRemoveLiquidity2 name actionSpaceAdd actionSpaceRemove factor =
  [opengame|

    inputs    :  state ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state ;
    feedback  :   ;
    operation :  addLiquidityGame name actionSpaceAdd ;
    outputs   :  addedLiquidity ;
    returns   :  paymentsAdd  + paymentsPrivate;

    inputs    :  state, addedLiquidity ;
    feedback  :   ;
    operation :  forwardFunction $ addLiquidity name ;
    outputs   :  state2 ;
    returns   :  ;

    inputs    :  state, state2 ;
    feedback  :   ;
    operation :  computePayments name ;
    outputs   :  paymentsAdd;
    returns   :   ;

    inputs    :  state2 ;
    feedback  :   ;
    operation :  removeLiquidityGame name actionSpaceRemove ;
    outputs   :  removedLiquidity ;
    returns   :  paymentsRemove ;

    inputs    :  state2, removedLiquidity ;
    feedback  :   ;
    operation :  forwardFunction $ removeLiquidity name ;
    outputs   :  state3 ;
    returns   :  ;

    inputs    :  state2, state3 ;
    feedback  :   ;
    operation :  computePayments name ;
    outputs   :  paymentsRemove ;
    returns   :   ;

    inputs    :  addedLiquidity ;
    feedback  :   ;
    operation :  forwardFunction $ addPrivateValue factor ;
    outputs   :  paymentsPrivate ;
    returns   :   ;


    :---------------------------:

    outputs   :  state3, paymentsAdd, paymentsRemove ;
    returns   :   ;
  |]

exampleSwap name1 name2 poolName1 poolName2 actionSpaceSwap1 (actionSpaceSwap2fst, actionSpaceSwap2snd) = [opengame|

    inputs    :  state ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state ;
    feedback  :  ;
    operation :  swapGame name1 actionSpaceSwap1 ;
    outputs   :  swapped1 ;
    returns   :  payment1 ;

    inputs    :  state, swapped1 ;
    feedback  :   ;
    operation :  forwardFunction $ swapAssets name1 ;
    outputs   :  state2 ;
    returns   :  ;

    inputs    :  state, state2 ;
    feedback  :   ;
    operation :  computePayments name1 ;
    outputs   :  payment1 ;
    returns   :   ;

    inputs    :  state2 ;
    feedback  :   ;
    operation :  swapGame name2 actionSpaceSwap2fst ;
    outputs   :  swapped2 ;
    returns   :  0 ;

    inputs    :  state2, swapped2 ;
    feedback  :  ;
    operation :  forwardFunction $ swapAssets name2 ;
    outputs   :  state3 ;
    returns   :  ;

    inputs    :  state3 ;
    feedback  :  ;
    operation :  swapGame name2 actionSpaceSwap2snd ;
    outputs   :  swapped3 ;
    returns   :  payment4 ;

    inputs    :  state3, swapped3 ;
    feedback  :  ;
    operation :  forwardFunction $ swapAssets name2 ;
    outputs   :  state4 ;
    returns   :  ;

    inputs    :  state2, state4 ;
    feedback  :   ;
    operation :  computePayments name2 ;
    outputs   :  payment4 ;
    returns   :   ;

    :---------------------------:

    outputs   :  state4, payment1, payment4 ;
    returns   :  ;
  |]
