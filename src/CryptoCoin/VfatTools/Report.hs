{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.VfatTools.Report where

-- Report out analysis results on yield-farming

import Data.Char (toUpper)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Data.Time (Day)

import Data.CryptoCurrency.Utils (pass')

import CryptoCoin.VfatTools.Types
          (YieldFarm(YieldFarm), YFOutput(YFOut), output, mkYFOutput, coins, yf)

-- 1HaskellADay modules:

import Control.List (weave)
import Control.Presentation (laxmi)

import Data.Monetary.USD (USD(USD))

reportYields :: String -> Day -> [YieldFarm] -> IO ()
reportYields coin date yfs =
     let capCoin = map toUpper coin
         title = capCoin ++ " yield-farm report for " ++ show date ++ ":"
         row = "rank,lp,tvl," ++ coin ++ " per week," ++ coin ++ "/$100/week"
         caveat = " (ranked highest-yield first)\n\n"
         alles = sortOn (Down . output) (map mkYFOutput yfs)
         coinPrices' = Map.unions (map (coins . yf) alles) -- for Sushi
         price = coinPrices' Map.! capCoin
         top10 = take 10 alles
         coinPrices = Map.unions (map (coins . yf) top10) in
     putStrLn (title ++ caveat ++ row ++ ",USD/$100/week") >>
     mapM (ppYieldFarm price) (zip [1..] top10)          >>=
     pass' (reportPrices date coinPrices)                >>=
     tweetIt date capCoin

ppYieldFarm :: USD -> (Int, YFOutput) -> IO String
ppYieldFarm (USD coinPrice) (show -> n, YFOut (YieldFarm nm _ t j) o) =
   let toks = toRational o
       dallahs = show (USD (toks * coinPrice)) in
   putStrLn (weave [n, nm, show t, show j, laxmi 2 toks, dallahs]) >> return nm

reportPrices :: Day -> Map String USD -> IO ()
reportPrices (show -> date) cns =
   putStrLn "\nCoin prices\n\nfor_date,cmc_id,coin,price (USD)" >>
   mapM_ (putStrLn . weave . ([date,""] ++) . tup2list) (Map.toList cns)

tup2list :: Show a => (String, a) -> [String]
tup2list (a,b) = [a, show b]

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

tweetIt :: Day -> String -> [String] -> IO ()
tweetIt date (('$':) -> coin) lps =
   mapM_ putStrLn
     ["", 
      "\"What is the LP that yields the most " ++ coin ++ " per USD?\"",
      "",
      head lps ++ " is the top " ++ coin ++ "-yielding LP for " ++ show date,
      "",
      "data scraped from: https://vfat.tools/","",
      "yield analyzer source code at https://github.com/logicalgraphs/CryptoCoin/tree/master/src/CryptoCoin/VfatTools"]
