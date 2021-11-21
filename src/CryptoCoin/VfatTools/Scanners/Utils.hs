module CryptoCoin.VfatTools.Scanners.Utils where

import Control.Arrow (second, (>>>))

import Data.Maybe (catMaybes)

import CryptoCoin.VfatTools.Types (YieldFarm)
import CryptoCoin.VfatTools.Scanners.Types (FileScanner)

readFarmsWith :: FileScanner -> FilePath -> IO [YieldFarm]
readFarmsWith scanner file =
   catMaybes . scanFileWith scanner . lines <$> readFile file

scanFileWith :: FileScanner -> [String] -> [Maybe YieldFarm]
scanFileWith scanner [] = []
scanFileWith scanner lines@(_:_) =
   (second (scanFileWith scanner) >>> uncurry (:)) (scanner lines)
