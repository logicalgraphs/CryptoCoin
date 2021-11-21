{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

module CryptoCoin.VfatTools.Scanners.Utils where

import Control.Arrow (second, (>>>))

import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)

import CryptoCoin.VfatTools.Types (YieldFarm(YieldFarm), CoinPrices)
import CryptoCoin.VfatTools.Scanners.Types
         (FileScanner, readCleenUSD, LPScanner)

-- 1HaskellADay imports

import Control.Scan.CSV (readMaybe)

readFarmsWith :: FileScanner -> FilePath -> IO [YieldFarm]
readFarmsWith scanner file =
   catMaybes . scanFileWith scanner . lines <$> readFile file

scanFileWith :: FileScanner -> [String] -> [Maybe YieldFarm]
scanFileWith scanner [] = []
scanFileWith scanner lines@(_:_) =
   (second (scanFileWith scanner) >>> uncurry (:)) (scanner lines)

fastForwardP :: (String -> Bool) -> [String] -> [String]
fastForwardP f [] = []
fastForwardP f lines@(l:ines) = if f l then lines else fastForwardP f ines

scanCoins :: String -> String -> CoinPrices
scanCoins a b = Map.fromList (mapMaybe scanCoin [a,b])

scanCoin :: LPScanner
scanCoin (words -> [c,_,v]) = (c,) <$> readCleenUSD v

convertYieldFarm ::
   Int -> String -> String -> String -> String -> Maybe YieldFarm
convertYieldFarm offset t a b jpw =
   let (n:r) = drop offset (words t)
       (_:_:_:j:_) = words jpw
   in  YieldFarm n (scanCoins a b) <$> readCleenUSD (last r) <*> readMaybe j
   -- in  if yf == Nothing then error (unwords ["Couldn't read",t,jpw]) else yf
