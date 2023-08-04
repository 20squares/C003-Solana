module Analytics where

import ActionSpaces
import Model
import OpenGames.Engine.Engine hiding (Payoff)
import OpenGames.Preprocessor
import Payoffs
import SupportFunctions
import Types

{-
Contains the basic analytics to be executed
There are two type of analyses:
1. (Bayesian) Nash eq. checks
2. Simulations
-}

--------------------------
-- 1. Equilibrium checking
--------------------------
  -- Equilibrium notiton for basic add/remove liquidity example game
equilibriumExampleAddRemoveLiquidity state name actionSpaceAdd actionSpaceRemove strategy = evaluate (exampleAddRemoveLiquidity name actionSpaceAdd actionSpaceRemove) strategy ctxt
 where
   ctxt = StochasticStatefulContext (pure ((),(state))) (\_ _ -> pure ())

printEquilibriumExampleAddRemoveLiquidity state name actionSpaceAdd actionSpaceRemove strategy = generateIsEq $ equilibriumExampleAddRemoveLiquidity state name actionSpaceAdd actionSpaceRemove strategy

-- Equilibrium notion for basic add/remove liquidity example game with private values
equilibriumExampleAddRemoveLiquidity2 state name actionSpaceAdd actionSpaceRemove factor strategy = evaluate (exampleAddRemoveLiquidity2 name actionSpaceAdd actionSpaceRemove factor) strategy ctxt
 where
   ctxt = StochasticStatefulContext (pure ((),(state))) (\_ _ -> pure ())

printEquilibriumExampleAddRemoveLiquidity2 state name actionSpaceAdd actionSpaceRemove factor strategy = generateIsEq $ equilibriumExampleAddRemoveLiquidity2 state name actionSpaceAdd actionSpaceRemove factor strategy

-- Equilibrium notion for basic swap example game
equilibriumExampleSwap state name1 name2 poolName1 poolName2 actionSpaceSwap1 (actionSpaceSwap2fst, actionSpaceSwap2snd) strategy = evaluate (exampleSwap name1 name2 poolName1 poolName2 actionSpaceSwap1 (actionSpaceSwap2fst, actionSpaceSwap2snd)) strategy ctxt
 where
   ctxt = StochasticStatefulContext (pure ((),(state))) (\_ _ -> pure ())

printEquilibriumExampleSwap state name1 name2 poolName1 poolName2 actionSpaceSwap1 (actionSpaceSwap2fst, actionSpaceSwap2snd) strategy = generateOutput $ equilibriumExampleSwap state name1 name2 poolName1 poolName2 actionSpaceSwap1 (actionSpaceSwap2fst, actionSpaceSwap2snd) strategy

-- Equilibrium notion for basic swap example game with private value and exogenous price update
equilibriumExampleSwapExogenousPriceChange state name1 name2 poolName1 poolName2 actionSpaceSwap1 (actionSpaceSwap2fst, actionSpaceSwap2snd) factor assetName priceChange1 strategy = evaluate (exampleSwapExogenousPriceChange name1 name2 poolName1 poolName2 actionSpaceSwap1 (actionSpaceSwap2fst, actionSpaceSwap2snd) factor assetName priceChange1) strategy ctxt
 where
   ctxt = StochasticStatefulContext (pure ((),(state))) (\_ _ -> pure ())

printEquilibriumExampleSwapExogenousPriceChange state name1 name2 poolName1 poolName2 actionSpaceSwap1 (actionSpaceSwap2fst, actionSpaceSwap2snd) factor assetName priceChange1 strategy = generateIsEq $ equilibriumExampleSwapExogenousPriceChange state name1 name2 poolName1 poolName2 actionSpaceSwap1 (actionSpaceSwap2fst, actionSpaceSwap2snd) factor assetName priceChange1 strategy


-----------------
-- 2. Simulations
-----------------
