{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.CoinMarketCap.Reports.Table where

import Data.List (sortOn)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Time (Day)

import CryptoCoin.CoinMarketCap.Types hiding (idx)
import CryptoCoin.CoinMarketCap.Types.Quote

import Data.CryptoCurrency.Types (Idx, rank, idx, Rank)

import Data.Monetary.USD
import Data.Percentage

import Data.XHTML hiding (P)

data PriceCoin = PC { ecoin :: ECoin, price1 :: Double, delta :: Double }
   deriving (Eq, Ord, Show)

instance Rank PriceCoin where
   rank = rank . ecoin

l2PC :: Listing -> Maybe PriceCoin
l2PC l = quote l >>= \q -> PC (coin l) (price q) <$> percentChange24h q

data ContextCoin = CC (Map Idx Listing) PriceCoin
   deriving (Eq, Ord, Show)

instance Rank ContextCoin where
   rank (CC _ pc) = rank pc

ec2cc :: MetaData -> ECoin -> Maybe ContextCoin
ec2cc (MetaData _ m) c = CC m <$> (Map.lookup (idx c) m >>= l2PC)

instance Rasa ContextCoin where
   printRow (CC _ pc@(PC (C (Coin ci)) p ch)) = tr (ci2tr ci p ch ++ [S "--"])
   printRow (CC m pc@(PC (T (Token ci i _)) p ch)) =
      tr (ci2tr ci p ch ++ [link . info . coin $ m Map.! i])
   
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

report :: Rank r => Rasa r => String -> [String] -> [r] -> IO ()
report typ hdrs razz =
   let sz     = length razz
       header = concat ["There",toBe sz,show sz," new ",typ,plural sz," today"]
       ranked = sortOn rank razz
   in  report' (header ++ punct sz) hdrs ranked

report' :: Rank r => Rasa r => String -> [String] -> [r] -> IO ()
report' msg hdrs razz = printContent (p [S msg]) 0 >> t' razz hdrs

punct :: Int -> String
punct sz | sz == 0 = "."
         | otherwise = ":"

p :: [Content] -> Content
p = E . Elt "p" []

newCoins :: MetaData -> NewCoins -> IO ()
newCoins md ncs@(coins, tokens) =
   mapM_ (uncurry (newStuff md)) [("coin", coins), ("token", tokens)]

coinHeaders :: [String]
coinHeaders = words "Name Symbol Rank Price %Change Basis"

newStuff :: MetaData -> String -> [ECoin] -> IO ()
newStuff md typ = report typ coinHeaders . mapMaybe (ec2cc md)

plural :: Int -> String
plural 1 = ""
plural _ = "s"

toBe :: Int -> String
toBe count | count == 1 = " is "
           | otherwise  = " are "
