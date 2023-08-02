{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Map.Strict
import Optics.TH

{-
Describes the types of the model and interfaces to the outside world
-}

-- Basic types
-- For now we will limit ourselves to a single instance of Account, Pool and Asset
-- But types are more general for possible future extensions
type AccountName = String

type PoolName = String

type AssetName = String

type AssetQuantity = Double

type Price = Double

data Side = Long | Short deriving (Show)

-- | Asset ratios
data Ratios = Ratios
  { targetRatio :: Double,
    minRatio :: Double,
    maxRatio :: Double
  }
  deriving (Show, Eq)

-- Fees for trading
data Fees = Fees
  { openPositionFee :: Double,
    closePositionFee :: Double,
    addLiquidityFee :: Double,
    removeLiquidityFee :: Double,
    swapFee :: Double,
    liquidationFee :: Double,
    ratioMultiplier :: Double
  }
  deriving (Show, Eq, Ord)

data PricingParams = PricingParams
  { swapSpread :: Double
  }
  deriving (Show, Eq)

-- State type
data State = State
  { accounts :: Map AccountName Account,
    pools :: Map PoolName Pool,
    externalPriceIndex :: Map AssetName Price
  }
  deriving (Show, Eq)

-- Account type
data Account = Account
  { -- | the lamports currently held into the account
    lamports :: Int,
    assets :: Map AssetName AssetQuantity,
    -- | the current long positions
    longPosition :: Map (PoolName, AssetName) AssetQuantity,
    -- | the current short positions
    shortPosition :: Map (PoolName, AssetName) AssetQuantity,
    collateralizedAssets :: Map AssetName AssetQuantity
  }
  deriving (Show, Eq)

-- Pool type
data Pool = Pool
  { -- | the current available GLP supply
    outstandingSupply :: AssetQuantity,
    -- | the available assets and their representation in the pool
    lpAssets :: Map AssetName AssetQuantity,
    -- | the current available asset prices denominated in numeraire
    priceIndex :: Map AssetName Price,
    -- | asset ratios
    assetRatios :: Map AssetName Ratios,
    -- | fees for each operation
    fees :: Fees,
    -- | Pricing parameters
    pricingParams :: PricingParams
  }
  deriving (Show, Eq)
