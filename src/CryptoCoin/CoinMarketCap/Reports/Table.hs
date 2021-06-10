module CryptoCoin.CoinMarketCap.Reports.Table where

import Data.List (sortOn)
import Data.Time (Day)

import Control.List (weave)
import Control.Presentation (Univ, explode)

import Data.CryptoCurrency.Types (rank, Rank)
import Data.CryptoCurrency.Utils (plural, toBe)

csvReport :: Rank r => Univ r => Day -> String -> [String] -> [r] -> IO ()
csvReport date typ hdrs = uncurry (csvReport' hdrs) . setupReport date typ

csvReport' :: Univ r => [String] -> String -> [r] -> IO ()
csvReport' hdrs title sorted =
   putStrLn (title ++ "\n") >>
   mapM_ (putStrLn . weave) (hdrs:map explode sorted)

setupReport :: Rank r => Day -> String -> [r] -> (String, [r])
setupReport date typ razz =
   let sz     = length razz
       header = concat ["There",toBe sz,show sz," new ",typ,plural sz]
       ranked = sortOn rank razz
   in  (weave [show date, header], ranked)  -- this is now CSV-y. Oh, well.

coinHeaders :: [String]
coinHeaders = words "Name Symbol Rank Price %Change Basis"
