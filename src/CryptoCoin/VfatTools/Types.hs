module CryptoCoin.VfatTools.Types where

import Data.Map (Map)

import Control.Scan.CSV (readMaybe)

import Data.Monetary.USD

type Coin = String

type CoinPrices = Map Coin USD

data YieldFarm =
   YieldFarm { name   :: String,
               coins  :: CoinPrices,
               tvl    :: USD,
               jewels :: Double }
      deriving (Eq, Ord, Show)

jewelsPer100Dollar :: YieldFarm -> Double
jewelsPer100Dollar (YieldFarm _ _ (USD d) j ) = j * 100 / fromRational d

data YFOutput = YFOut { yf :: YieldFarm, output :: Double }
   deriving (Eq, Ord, Show)

mkYFOutput :: YieldFarm -> YFOutput
mkYFOutput = YFOut <*> jewelsPer100Dollar
