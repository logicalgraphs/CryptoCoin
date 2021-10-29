{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.Oracle.BNBPredictor where

{--
We guess the price, Up or Down, in 5 minutes based upon priors

So:

We need to ingest priors
Build a model
Output prediction.

Grade

Feedback.

Then, let's see how we do
--}

import Control.Comonad ((=>>))
import Control.Monad (foldM)

import Data.Char (toUpper)
import Data.Maybe (mapMaybe)
import Data.Time (Day)

import Control.List (softtail)
import Control.Scan.CSV (csv)
import CryptoCoin.Utils (dateDir)
import Data.Monetary.USD
import Data.Percentage
import Data.Time.TimeSeries (today)

data Call = U | D
   deriving (Eq, Ord, Show, Read)

-- list-as-stack: current price on top of stack (so we go 'backwards' in time)

readPrice :: Char -> Call
readPrice = read . return . toUpper

sample :: [Call]          -- present --v     v-- first result
sample = map readPrice "ddduududddduduuudduud"

val :: Call -> Double
val U = 1
val D = -1

type Confidence = Percentage

predict :: [Call] -> Confidence     -- exponential?
predict = P . toRational . p' 0.5 0

p' :: Double -> Double -> [Call] -> Double
p' mult sum [] = sum
p' mult sum (h:t) = p' (mult / 2) (val h * mult + sum) t

{--
with "duuudduud", predicted -9.9609375e-2 (D), was: u
with "uduuudduud", predicted 0.4501953125 (u), was: d
with "duduuudduud", predicted -28.50% (d), was: d
with "dduduuudduud", predicted -64.25% (d), was: d
with "ddduduuudduud", predicted -82.12% (d), was: d
with "uddduduuudduud", predicted 4.53% (u), was: d
with "duddduduuudduud", predicted -48.26% (d), was: u
with "ududdduduuudduud", predicted 26.13% (u), was: u
with "uududdduduuudduud", predicted 63.06% (u), was: d
with "duududdduduuudduud", predicted -19.53% (d), was: d
with "dduududdduduuudduud", predicted -60.76% (d), was: d
with "ddduududdduduuudduud", predicted -80.38% (d), was: u
with "uddduududdduduuudduud", predicted 10.19% (u), was: d
--}

readHist :: FilePath -> IO [Call]
readHist file = map (readPrice . last) . tail . lines <$> readFile file

runDate :: Day -> IO ()
runDate date =
     dateDir "updowns" date      >>=
     readHist . (++ "/hist.csv") >>=
     print . predict

go :: IO ()
go = today >>= runDate

-- PLAY STRATEGIES -------------------------------------------------------

type Bag = (USD, USD)

type StratFn = [Call] -> Bag -> Confidence -> Bag

play :: [Call] -> (Confidence, Call) -> StratFn -> Bag -> Bag
play hist cc@(P p, c) f bag@(USD b1, USD b2) =
   let (USD ante1, USD ante2) = f (softtail hist) bag (P p)
       newbag = (b1 - ante1, b2 - ante2)
       payout a m = a * (1 + (1.75 * toRational (pl (fromRational p * m * val c))))
       f' nb a m = USD (nb + payout a m)
   in  (f' (fst newbag) ante1 1, f' (snd newbag) ante2 (-1))

pl :: Double -> Double
pl n = if n > 0 then 1 else 0

single10, flat, both, sure1, sure2, runOf2 :: StratFn
single10 _ (USD b, _) (P conf) = (USD $ b / 10 * conf, USD 0)
flat _ (USD b, _) _ = (USD $ b / 10, USD 0)
both _ (USD b, USD b') (P conf) =
   let f amt p = USD (amt / 10 * p) in (f b conf, f b' (1 - conf))
sure1 _ (USD a, _) (P conf) = (USD $ (if conf > 0.5 then a else 0) / 10, USD 0)
sure2 _ (USD a, USD b) (P conf) =
   let f amt p = USD ((if p > 0.5 then amt else 0) / 10 * p)
   in  (f a conf, f b (1 - conf))

-- runs in updowns:

runOf2 (a:b:_) (USD c, _) _ = 
   let call = if a == b then c / 10 else 0 in (USD call, USD 0)
runOf2 _ _ _ = (USD 0, USD 0)

runCOf, runOf, runBothOf, runBothCOf :: Int -> StratFn
runCOf _ [] _ _ = (USD 0, USD 0)
runCOf n list@(h:_) (USD c, _) (P conf) = 
   let samies = take n list
   in  (USD $ if all (h==) samies then c / 10 * conf else 0, USD 0)

runOf n l b _ = runCOf n l b (P 1)

runBothCOf _ [] _ _ = (USD 0, USD 0)
runBothCOf n list@(h:_) (USD c1, USD c2) (P conf) = 
   let samies = take n list
       f a = USD (if all (h==) samies then a / 10 * conf else 0)
   in  (f c1, f c2)

runBothOf n l b _ = runBothCOf n l b (P 1)

-- execution engine:

runStrat :: StratFn -> Bag -> FilePath -> Day -> IO Bag
runStrat f bag file date =
   dateDir "updowns" date     >>=
   readConf . (++ ('/':file)) >>= \ccs ->
   putStrLn ("Start: " ++ show (fst bag + snd bag)) >>
   snd <$> foldM (uncurry (printRow f))
                 (reverse $ map snd ccs =>> id, bag) (reverse ccs) >>= \ans ->
   putStrLn ("End: " ++ show (fst ans + snd ans)) >>
   return ans

printRow :: StratFn -> [[Call]] -> Bag -> (Confidence, Call) -> IO ([[Call]], Bag)
printRow f c b cc =
   let yo = play (head c) cc f b in
   putStrLn (concat ["Comin' in at ", show b, ". And: ", show yo]) >>
   return (tail c, yo)

readConf :: FilePath -> IO [(Confidence, Call)]
readConf file = mapMaybe rc . reverse . tail . lines <$> readFile file

rc :: String -> Maybe (Confidence, Call)
rc = rc' . csv

rc' :: [String] -> Maybe (Confidence, Call)
rc' [_,c,(h:_)] = Just (read c, readPrice h)
rc' [_, _] = Nothing

{--
Example runs:

>>> runStrat sure2 (USD 1000, USD 1000) "hist.csv" (read "2021-10-22")

runOf: 3 looks like we can try that.
--}
