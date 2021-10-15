{-# LANGUAGE TupleSections #-}

module CryptoCoin.DefiKingdoms.VfatTools.TxtScanner where

-- We read in the DefiKingdoms txt file generated by vfat.tools

import Control.Arrow

import Data.List (isPrefixOf, dropWhile, sortOn, intercalate)
import Data.Maybe (catMaybes)
import Data.Ord

import Control.Scan.CSV (readMaybe)

import CryptoCoin.Utils (dateDir)

import Data.Monetary.USD
import Data.Time.TimeSeries (today)

data YieldFarm =
   YieldFarm { name :: String, tvl :: USD, jewels :: Double }
      deriving (Eq, Ord, Show)

readFarms :: FilePath -> IO [YieldFarm]
readFarms file =
   readFile file >>= return . catMaybes . scanFile . lines

fastForwardTo :: String -> [String] -> [String]
fastForwardTo _ [] = []
fastForwardTo start lines@(l:ines)
   | l == []                     = fastForwardTo start ines
   | start `isPrefixOf` (dropWhile (/= ' ') l) = lines
   | otherwise                   = fastForwardTo start ines

{--
First iteration of scanFile where YieldFarm = String:

>>> today >>= dateDir "kingdoms" >>= readFarms . (++ "/scrape.txt") >>= mapM_ putStrLn . take 5
0 - [JEWEL]-[WONE] Uni LP [+] [-] [<=>] Price: $2.02 TVL: $51,097,846.16
JEWEL Price: $4.40n
WONE Price: $0.22
Staked: 24970606.4902 JEWEL-LP ($50,340,218.91)
JEWEL Per Week: 7294070.35 ($32,088,439.83)
So, a YieldFarm is the one that provides the most JEWEL / USD
--}

scanFile :: [String] -> [Maybe YieldFarm]
scanFile [] = []
scanFile lines@(_:_) =
   (second scanFile >>> uncurry (:))
    (convertOneBlock (fastForwardTo " - [" lines))

convertOneBlock :: [String] -> (Maybe YieldFarm, [String])
convertOneBlock [] = (Nothing, [])
convertOneBlock (t:_:_:_:jpw:rest) = (convertYieldFarm t jpw, rest)

convertYieldFarm :: String -> String -> Maybe YieldFarm
convertYieldFarm t jpw =
   let l@(_:_:n:r) = words t
       j@(_:_:_:a:_) = words jpw
   in  YieldFarm n <$> readMaybe (filter (/= ',') (last r)) <*> readMaybe a
   -- in  if yf == Nothing then error (unwords ["Couldn't read",t,jpw]) else yf

{--
With those 3 new functions, we now have:

>>> today >>= dateDir "kingdoms" >>= readFarms . (++ "/scrape.txt") >>= mapM_ print
YieldFarm {name = "[JEWEL]-[WONE]", tvl = $51097846.16, jewels = 7294070.35}
YieldFarm {name = "[JEWEL]-[BUSD]", tvl = $3789566.95, jewels = 607839.2}
YieldFarm {name = "[JEWEL]-[bscBNB]", tvl = $3238734.51, jewels = 607839.2}
YieldFarm {name = "[1ETH]-[JEWEL]", tvl = $4208499.00, jewels = 607839.2}
YieldFarm {name = "[WONE]-[BUSD]", tvl = $5476445.84, jewels = 121567.84}
YieldFarm {name = "[JEWEL]-[XYA]", tvl = $1717237.62, jewels = 303919.6}
YieldFarm {name = "[JEWEL]-[1USDC]", tvl = $3906428.91, jewels = 607839.2}
YieldFarm {name = "[1WBTC]-[JEWEL]", tvl = $4035600.15, jewels = 607839.2}
YieldFarm {name = "[UST]-[JEWEL]", tvl = $3434142.36, jewels = 607839.2}
YieldFarm {name = "[1ETH]-[WONE]", tvl = $6290152.85, jewels = 121567.84}
--}

jewelsPerDollar :: YieldFarm -> Double
jewelsPerDollar (YieldFarm _ (USD d) j) = j / fromRational d

data YFOutput = YFOut { yf :: YieldFarm, output :: Double }
   deriving (Eq, Ord, Show)

mkYFOutput :: YieldFarm -> YFOutput
mkYFOutput = YFOut <*> jewelsPerDollar

go :: IO ()
go = today                             >>= \tday ->
     let title = "JEWEL yield-farm report for " ++ show tday ++ ":"
         caveat = " (ranked highest-yield first)\n\n"
         row = "lp,tvl,jewels per week,jewels/dollar/week yield" in
     putStrLn (title ++ caveat ++ row) >>
     dateDir "kingdoms" tday           >>=
     readFarms . (++ "/scrape.txt")    >>=
     mapM_ ppYieldFarm . sortOn (Down . output) . map mkYFOutput

ppYieldFarm :: YFOutput -> IO ()
ppYieldFarm (YFOut (YieldFarm n t j) o) =
   putStrLn (intercalate "," [n,show t, show j, show o])

{--
The result of which is:

>>> go
JEWEL yield-farm report for 2021-10-15: (ranked highest-yield first)

lp,tvl,jewels per week,jewels/dollar/week yield
[JEWEL]-[bscBNB],$3238734.51,607839.2,0.18767799496526502
[UST]-[JEWEL],$3434142.36,607839.2,0.17699883563213897
[JEWEL]-[XYA],$1717237.62,303919.6,0.1769816800980004
[JEWEL]-[BUSD],$3789566.95,607839.2,0.16039806321353145
[JEWEL]-[MIS],$1536246.69,243135.68,0.1582660394266951
[JEWEL]-[1USDC],$3906428.91,607839.2,0.1555997083848126
[1WBTC]-[JEWEL],$4035600.15,607839.2,0.15061928254881832
[1ETH]-[JEWEL],$4208499.00,607839.2,0.14443135171894855
[JEWEL]-[WONE],$51097846.16,7294070.35,0.14274711944700613
[1SUPERBID]-[JEWEL],$1203858.00,109411.06,9.088369188524024e-2
[1SUPERBID]-[WONE],$326842.99,12156.78,3.7194555752984707e-2
[WONE]-[BUSD],$5476445.84,121567.84,2.219830953153523e-2
[1USDC]-[WONE],$5542266.80,121567.84,2.193467840457024e-2
[1ETH]-[WONE],$6290152.85,121567.84,1.9326690909167103e-2
[1WBTC]-[1ETH],$34686433.67,121567.84,3.504766190120581e-3
--}
