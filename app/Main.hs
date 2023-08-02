module Main where

import ActionSpaces
import Analytics
import Mock
import Strategies

testStrategy1 = strategyTuple ("Test", "Sol", 10, 1) ("Test", "Sol", 909089.86, 1)
testStrategy2 = strategyTuple ("Test", "Sol", 0, 100) ("Test", "Sol", 909089.86, 1)

main = do
  printEquilibriumExampleAddRemoveLiquidity state1 "Aki" (const actionSpaceAddLiquidity) (const actionSpaceRemoveLiquidity) testStrategy1
  printEquilibriumExampleAddRemoveLiquidity2 state1 "Aki" (const actionSpaceAddLiquidity) (const actionSpaceRemoveLiquidity) 8 testStrategy1
