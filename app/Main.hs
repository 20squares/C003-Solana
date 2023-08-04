module Main where

import ActionSpaces
import Analytics
import Mock
import Strategies

testStrategy1 = strategyTupleAddRemoveLiquidity ("Test", "Sol", 10, 1) ("Test", "Sol", 909089.86, 1)
testStrategy2 = strategyTupleAddRemoveLiquidity ("Test", "Sol", 0, 100) ("Test", "Sol", 909089.86, 1)
testStrategy3 = strategyTupleSwap ("Pool1", "Sol", "Doge", 10000000, 0) ("Test", "Sol", "Doge", 0, 100) ("Pool1", "Sol", "Doge", 10000000, 0)
  

main = do
  printEquilibriumExampleAddRemoveLiquidity state1 "Aki" (const actionSpaceAddLiquidity) (const actionSpaceRemoveLiquidity) testStrategy1
  printEquilibriumExampleAddRemoveLiquidity2 state1 "Aki" (const actionSpaceAddLiquidity) (const actionSpaceRemoveLiquidity) 8 testStrategy1
  printEquilibriumExampleSwap state3 "Aki" "Bogdan" "Pool1" "Pool2" actionSpaceSwap actionSpaceArb testStrategy3
 -- printEquilibriumExampleSwapExogenousPriceChange state name1 name2 poolName1 poolName2 actionSpaceSwap1 (actionSpaceSwap2fst, actionSpaceSwap2snd) factor assetName priceChange1 strategy
