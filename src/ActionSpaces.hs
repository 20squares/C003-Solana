module ActionSpaces where

import Types
import Mock

{-
Describes the action space that the players have access to
-}

actionSpaceAddLiquidity, actionSpaceRemoveLiquidity :: [(PoolName, AssetName, AssetQuantity, AssetQuantity)]
actionSpaceAddLiquidity = [("Test", "Sol", 10, 1), ("Test", "Sol", 0, 100)]
actionSpaceRemoveLiquidity = [("Test", "Sol", 909089.86, 1), ("Test", "Sol", 0, 100)]
