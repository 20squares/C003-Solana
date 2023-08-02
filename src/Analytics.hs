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
-- Equilibrium notiton for reporter game
equilibriumExampleAddRemoveLiquidity state name actionSpaceAdd actionSpaceRemove strategy = evaluate (exampleAddRemoveLiquidity name actionSpaceAdd actionSpaceRemove) strategy ctxt
 where
   ctxt = StochasticStatefulContext (pure ((),(state))) (\_ _ -> pure ())

printEquilibriumExampleAddRemoveLiquidity state name actionSpaceAdd actionSpaceRemove strategy = generateIsEq $ equilibriumExampleAddRemoveLiquidity state name actionSpaceAdd actionSpaceRemove strategy


-----------------
-- 2. Simulations
-----------------
