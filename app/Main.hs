module Main where

import ActionSpaces
import Analytics
import Mock
import Strategies

testStrategy1 = strategyTupleAddRemoveLiquidity ("Test", "Sol", 10, 1) ("Test", "Sol", 909089.86, 1)
testStrategy2 = strategyTupleAddRemoveLiquidity ("Test", "Sol", 0, 100) ("Test", "Sol", 909089.86, 1)
testStrategy3 = strategyTupleSwap ("Pool2","Sol","Doge",0.0,100.0) ("Pool2","Sol","Doge",0.0,100.0) ("Pool2", "Sol", "Doge", 10000000, 0)


main = do
  -- printEquilibriumExampleAddRemoveLiquidity state1 "Aki" (const actionSpaceAddLiquidity) (const actionSpaceRemoveLiquidity) testStrategy1
  -- printEquilibriumExampleAddRemoveLiquidity2 state1 "Aki" (const actionSpaceAddLiquidity) (const actionSpaceRemoveLiquidity) 8 testStrategy1
  printEquilibriumExampleSwap state3 "Aki" "Bogdan" "Pool1" "Pool2" (const actionSpaceSwap) (const $ fst actionSpaceArb, const $ snd actionSpaceArb) testStrategy3
  -- printEquilibriumExampleSwapExogenousPriceChange state3 "Aki" "Bogdan" "Pool1" "Pool2" (const actionSpaceSwap) (const $ fst actionSpaceArb, const $ snd actionSpaceArb) 8 "Sol" 1.2 testStrategy3
  
