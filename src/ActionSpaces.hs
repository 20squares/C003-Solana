module ActionSpaces where

import Types
import Parameterization

{-
Describes the action space that the players have access to
-}

actionSpaceAddLiquidity, actionSpaceRemoveLiquidity :: [(PoolName, AssetName, AssetQuantity, AssetQuantity)]
actionSpaceAddLiquidity = [("Test", "Sol", 10, 1), ("Test", "Sol", 0, 100)]
actionSpaceRemoveLiquidity = [("Test", "Sol", 909089.86, 1), ("Test", "Sol", 0, 100)]

actionSpaceSwap :: [(PoolName, AssetName, AssetName, AssetQuantity, AssetQuantity)]
actionSpaceSwap = [("Pool1", "Sol", "Doge", 10000000, 0), ("Pool2", "Sol", "Doge", 0, 100)]

actionSpaceArb :: ([(PoolName, AssetName, AssetName, AssetQuantity, AssetQuantity)], [(PoolName, AssetName, AssetName, AssetQuantity, AssetQuantity)])
actionSpaceArb = ([("Pool1", "Doge", "Sol", 100000000, 0), ("Pool2", "Sol", "Doge", 0, 100)], [("Pool2", "Sol", "Doge", 10000000, 0), ("Pool1", "Sol", "Doge", 0, 100)])
