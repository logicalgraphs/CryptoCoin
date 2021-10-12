module Ingest where

-- Ingests a CSV-file of wallet-STE (StarTerra Energy) by faction
-- data collated at: https://raw.githubusercontent.com/starterra/tools/main/ste-staking-results/08102021-stt_staking_users.csv

import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Ord

-- below libraries are part of https://github.com/geophf/1HaskellADay repository

import Control.Map (snarfL)
import Control.Scan.CSV (csv, readMaybe)

type Wallet = String
type Faction = String

data STE = STE { wallet :: Wallet, lp, stt, ste :: Double, faction :: Faction }
   deriving (Eq, Ord, Show)

inDir, outDir :: FilePath
inDir = "ste-staking-results/"
outDir = "analyses/"

csvFile :: FilePath
csvFile = "08102021-stt_staking_users.csv"

-- reportage:

go :: IO ()
go = ingest (inDir ++ csvFile) >>= putStrLn . divvy

divvy :: [Either a b] -> String
divvy rows =
   "I processed " ++ show (rights rows) ++ " out of " ++ show (length rows)

rights :: [Either a b] -> Int
rights = length . filter isRight

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

-- pretty-printer:

ppf :: Show a => (String, a) -> String
ppf (a, b) = a ++ (',':show b)

-- parser:

ingest :: FilePath -> IO [Either String STE]
ingest file = map ingestRow . tail . lines <$> readFile file

ingestRow :: String -> Either String STE
ingestRow = ir' . csv

ir' :: [String] -> Either String STE
ir' r@[w,l,s0,s1,f] = maybe (ir' ("*":r)) Right (scan r)
ir' r = Left ("Could not process " ++ show r)

scan :: [String] -> Maybe STE
scan [w,l,s0,s1,f] =
   STE <$> Just w <*> readMaybe l <*> readMaybe s0 <*> readMaybe s1 <*> Just f

{--
With the parsed values we can do the following:

1. get number of data rows:

>>> rows <- ingest (inDir ++ csvFile)
>>> length rows
15180

2. divvy STE by faction:

>>> let facts = snarfL (\(Right (STE _w _l _s ste f)) -> Just (f, ste)) rows
>>> length facts
4

3. And see what the factions are:

>>> Map.keys facts
["MANY_FACTIONS","degens","interstellars","lunatics"]

4. What is all the STE in the StarTerra-verse?

>>> sum (map (\(Right (STE _ _ _ ste _)) -> ste) rows)
1.0572979021005254e7

5. What are the STE by faction?

>>> mapM_ (putStrLn . ppf) . Map.toList $ Map.map sum facts
MANY_FACTIONS,1038.74008275
degens,3338177.8205529996
interstellars,3392629.0503620044
lunatics,3841133.410007502

This CSV can be exported to https://observablehq.com/@d3/pie-chart and be
displayed as a pie-chart.

6. Who are the top-10 STE-stakers?

>>> let wallies = snarfL (\(Right (STE w _l _s ste f)) -> Just (f ++ (':':w), ste)) rows
>>> mapM_ (putStrLn . ppf) top10
lunatics:terra1qxg6s...w3r6,358121.4078515
degens:terra1ma8jq...jlpy,158025.47146875
lunatics:terra17vglw...rppm,155890.91507
degens:terra1hugk5...4luv,146386.44339375
interstellars:terra1glxyh...eff2,143552.39594375
lunatics:terra1w36en...up3r,135003.75822125
interstellars:terra1um84w...zmvz,108697.52991625
interstellars:terra144ucv...0slx,107078.25694125
interstellars:terra1qn93r...lc5u,102404.95185499999
degens:terra1tz9gy...86qg,93026.27244375

This CSV can be exported to https://observablehq.com/@d3/horizontal-bar-chart
and be displayed as a horizontal bar chart.
--}
