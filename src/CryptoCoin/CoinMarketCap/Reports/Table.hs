{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.CoinMarketCap.Reports.Table where

import Data.Foldable (toList)
import Data.List (sortOn)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Time (Day)

import Control.List (weave)
import Control.Presentation hiding (S)
import Control.Scan.CSV

import CryptoCoin.CoinMarketCap.Types hiding (idx)
import CryptoCoin.CoinMarketCap.Types.Quote

import Data.CryptoCurrency.Types (Idx, rank, idx, Rank, Indexed)
import Data.CryptoCurrency.Utils (plural, toBe, punct)

import Data.Monetary.USD
import Data.Percentage

import Data.XHTML hiding (P)

data PriceCoin = PC { ecoin :: ECoin, price1 :: Double, delta :: Double }
   deriving (Eq, Ord, Show)

instance Rank PriceCoin where
   rank = rank . ecoin

l2PC :: Listing -> Maybe PriceCoin
l2PC l = quote l >>= \q -> PC (coin l) (price q) <$> percentChange24h q

addlInfo :: Listings -> ECoin -> Content
addlInfo _ (C _) = S "--"
addlInfo m (T (Token _ci i _)) = link . info . coin $ m Map.! i
   
ci2tr :: CoinInfo -> Double -> Double -> [Content]
ci2tr c@(CoinInfo _i name sym slug rank _date) pr ch =
   [link c, S sym, S (show rank), s USD pr, s P (ch / 100)]
      where s f = S . show . f . toRational

link :: CoinInfo -> Content
link (CoinInfo _i name _sym slug _rank _date) =
   E $ linq slug name

linq :: String -> String -> Element
linq slug name = a (coinmarketcapcoinlink slug) name

a :: String -> String -> Element
a href = Elt "a" [Attrib "href" href] . return . S
   
coinmarketcaphref :: String
coinmarketcaphref = "https://coinmarketcap.com"
   
coinmarketcapcoinlink :: String -> String
coinmarketcapcoinlink = ((coinmarketcaphref ++ "/currencies/") ++) . (++ "/")

top10 :: Day -> Content
top10 (show -> date) =
   p [S (unwords ["The top-10 e-coins for",date,"(ranked by"]),
      E $ a coinmarketcaphref "coinmarketcap.com", S ") are:"]

table :: Rasa r => [String] -> [r] -> IO ()
table hdrs =
   flip printContent 3 . E
      . tabulate [Attrib "border" "1"] [thdrs hdrs]

t' :: Rasa r => [r] -> [String] -> IO ()
t' [] = const $ return ()
t' rs@(_:_) = flip table rs

report :: Rank r => Rasa r => Day -> String -> [String] -> [r] -> IO ()
report date typ hdrs =
   let r' a (b, c) = report' b a c in r' hdrs . setupReport date typ

-- alternatively, to output as CSV:

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

report' :: Rasa r => String -> [String] -> [r] -> IO ()
report' msg hdrs razz = printContent (p [S msg]) 0 >> t' razz hdrs

p :: [Content] -> Content
p = E . Elt "p" []

coinHeaders :: [String]
coinHeaders = words "Name Symbol Rank Price %Change Basis"
