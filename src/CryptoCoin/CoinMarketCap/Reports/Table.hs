{-# LANGUAGE ViewPatterns #-}

module CryptoCoin.CoinMarketCap.Reports.Table where

import Data.List (sortOn)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import Data.Time (Day)

import CryptoCoin.CoinMarketCap.Types hiding (idx)
import CryptoCoin.CoinMarketCap.Types.Quote

import Data.CryptoCurrency.Types (Idx, rank, idx)

import Data.Monetary.USD
import Data.Percentage

import Data.XHTML hiding (P)

data PriceCoin = PC { ecoin :: ECoin, price1 :: Double, delta :: Double }
   deriving (Eq, Ord, Show)

l2PC :: Listing -> Maybe PriceCoin
l2PC l = quote l >>= \q -> PC (coin l) (price q) <$> percentChange24h q

data ContextCoin = CC (Map Idx Listing) PriceCoin
   deriving (Eq, Ord, Show)

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

header :: Day -> IO ()
header (take 10 . show -> date) =
   printContent (p [S (unwords ["The top-10 e-coins for",date,"(ranked by"]),
                    E $ a coinmarketcaphref "coinmarketcap.com",
                    S ") are:"]) 0

table :: Rasa r => [String] -> [r] -> IO ()
table hdrs =
   flip printContent 3 . E
      . tabulate [Attrib "border" "1"] [thdrs hdrs]

report :: MetaData -> [ECoin] -> IO ()
report md =
   table (words "Name Symbol Rank Price %Change Basis") . mapMaybe (ec2cc md)

p :: [Content] -> Content
p = E . Elt "p" []

newCoins :: MetaData -> NewCoins -> IO NewCoins
newCoins md ncs@(coins, tokens) =
   newStuff md "coin" coins >> newStuff md "token" tokens >> return ncs

newStuff :: MetaData -> String -> [ECoin] -> IO ()
newStuff md typ = ns' md typ . id <*> length

ns' :: MetaData -> String -> [ECoin] -> Int -> IO ()
ns' _ _ _ 0 = return ()
ns' md typ coins _ =
   let sz     = length coins
       header = concat ["There",toBe sz,show sz," new ",typ,plural sz," today:"]
       ranked = sortOn rank coins
   in  printContent (p [S header]) 0 >> report md ranked

plural :: Int -> String
plural 1 = ""
plural _ = "s"

toBe :: Int -> String
toBe count | count == 1 = " is "
           | otherwise  = " are "
