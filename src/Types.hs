{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Types
  where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Optics.TH


{-
Describes the types of the model and interfaces to the outside world
-}


-- Basic types
type Price = Double

type AssetName = String

type AssetQuantity = Double

-- Fees for trading
data Fees = Fees
  { openPositionFee  :: Double
  , closePositionFee :: Double
  , executionFee     :: Double
  , swapFee          :: Double
  } deriving (Show,Eq,Ord)


-- State type
-- TODO we probably need more fields
data State = State
 { assetIndex     :: Map AssetName AssetQuantity -- ^ the available assets and their representation in the pool
 , priceIndex     :: Map AssetName Price         -- ^ the current available asset prices denominated in numeraire -- TODO check w
 , longPosition   :: Map AssetName AssetQuantity -- ^ the current long positions
 , shortPosition  :: Map AssetName AssetQuantity -- ^ the current short positions
 , outstandingGLP :: AssetQuantity               -- ^ the current available GLP supply
 }
