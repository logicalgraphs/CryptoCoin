{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Reports.Portfolio where

{--
Gives a report of the coins and the balances and the totals of each portfolio.

Woo, boy!
--}

import Control.Arrow ((&&&))
import Control.Monad (foldM)

import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Monoid (mconcat)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (Day, addDays)

import Database.PostgreSQL.Simple

import Control.Presentation hiding (S)
import CryptoCoin.CoinMarketCap.Reports.Table hiding (ecoin)
import CryptoCoin.CoinMarketCap.Types
import CryptoCoin.CoinMarketCap.Types.Quote

import Data.CryptoCurrency.Types
import Data.CryptoCurrency.Types.Coin (allCoinsLk)
import Data.CryptoCurrency.Types.Portfolio
import Data.CryptoCurrency.Types.Transaction
import Data.CryptoCurrency.Types.Transactions.Context
import Data.CryptoCurrency.Utils (pass)
import Data.LookupTable (LookupTable)
import Data.Monetary.USD
import Data.Percentage
import Data.Time.TimeSeries (today)
import Data.XHTML hiding (P)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed (IxValue(IxV))
import Store.SQL.Util.LookupTable (lookupTable, lookupTableFrom)

data Holding =
   Holding { ecoin :: ECoin, amount :: Double, invested, currentPrice :: USD }
      deriving (Eq, Ord, Show)

instance Indexed Holding where
   idx = idx . ecoin

instance Rank Holding where
   rank = rank . ecoin

data PortfolioReport = PR { portfolio     :: Portfolio,
                            holdings      :: Set Holding }
   deriving (Eq, Ord, Show)

instance Monoid PortfolioReport where
   mempty = PR (Portfolio "Nada" (USD 0) Nothing) Set.empty
   PR p0 h0 `mappend` PR p1 h1 =
       PR (Portfolio "All Portfolii" (cash p0 + cash p1) Nothing)
          (Set.union h0 h1)

totalInvested, totalValue :: PortfolioReport -> USD
totalInvested (PR _ holds) = sumOver invested holds
totalValue (PR _ holds) = sumOver val holds
   where val h = USD (toRational (amount h * doubledown (currentPrice h)))

toHolding :: Foldable t => Listings -> Idx -> t Transaction -> Maybe Holding
toHolding listings ix transes = 
   Map.lookup ix listings >>= \listing ->
   quote listing          >>= \quot ->
   return (buildHolding (coin listing) (price quot) transes)

buildHolding :: Foldable t => ECoin -> Double -> t Transaction -> Holding
buildHolding coin pric transes =
   let coins    = sumOver ncoins transes
       invested = sumOver spent transes
   in  Holding coin coins invested (USD (toRational pric))

sumOver :: Foldable t => Num b => (a -> b) -> t a -> b
sumOver f = sum . map f . toList

-- Now we build the table/rasa-representation of the portfolio

data KV = KV String String
   deriving (Eq, Ord, Show)

instance Rasa KV where
   printRow (KV k v) = tr [S (k ++ ":"), S v]

instance Univ KV where explode (KV k v) = [k, v]

printPortfolioReport :: Day -> PortfolioReport -> IO ()
printPortfolioReport date pr@(PR p holdings) =
   summarizePortfolio date pr >>
   report' ("Holdings for " ++ portfolioName p ++ " portfolio")
           (words "Rank Id Symbol Coin Amount Invested Price Value %change")
           (sortOn rank (Set.toList holdings))

data PorNot = Perc Percentage | PNOT
   deriving Eq

change :: Rational -> Rational -> PorNot
change inv val = if inv < 1 then PNOT else Perc (P ((val - inv) / inv))

instance Show PorNot where
   show (Perc p) = show p
   show PNOT = "N/A"
   
instance Rasa Holding where
   printRow h@(Holding coin amt ui@(USD inv) up@(USD pric)) =
      let currVal = toRational amt * toRational pric  in
      tr [s' (rank h), s' (idx h), S (sym coin), S (namei coin),
          s' amt, s' ui, s' up, s' (USD currVal), s' (change inv currVal)]
         where s' :: Show s => s -> Content
               s' = S . show

forEachPortfolioDo :: Connection -> Day -> TransactionContext
                   -> ([PortfolioReport], Listings) -> IxValue Portfolio
                   -> IO ([PortfolioReport], Listings)
forEachPortfolioDo conn tday tc (prs, lists) i@(IxV ix port) =
   fetchCoinIdsFor conn i                                     >>= \coinIds ->
   let newIds = Set.difference coinIds (Map.keysSet lists)
       portName = portfolioName port in
   fetchListings conn tday newIds                             >>= \newLists ->
   fetchTransactionsByPortfolio conn tc portName              >>= \transs ->
   let combinedListings = Map.union newLists lists
       coinsHeld = mapMaybe (uncurry (toHolding combinedListings))
                            (Map.toList transs)
   in  return (PR port (Set.fromList coinsHeld):prs, combinedListings)

buildSummary :: PortfolioReport -> [KV]
buildSummary pr@(PR (Portfolio name reserve _) _) =
   let ti@(USD totInv) = totalInvested pr
       tv@(USD totVal) = totalValue pr in
   [KV "Cash reserve" (show reserve),
    KV "Total invested" (show ti),
    KV "Current value" (show tv),
    KV "Gain/Loss" (show $ change totInv totVal)]

summarizePortfolio :: Day -> PortfolioReport -> IO ()
summarizePortfolio date pr@(PR (Portfolio name reserve _) holdings) =
   if holdings == Set.empty
   then printContent (p [S (unwords ["No holdings for", name, "portfolio."])]) 0
   else report' (unwords ["Summary of", name, "for", show date])
                [] (buildSummary pr)

sumport :: Day -> PortfolioReport -> IO ()
sumport date pr@(PR (Portfolio name _ _) holdings) =
   if holdings == Set.empty
   then putStrLn (unwords ["No holdings for", name, "portfolio."])
   else csvReport' [] (unwords ["Summary of", name, "for", show date])
                   (buildSummary pr)

summarizePortfolii :: Day -> [PortfolioReport] -> IO ()
summarizePortfolii date = summarizePortfolio date . mconcat

go :: IO ()
go = today >>= \tday ->
     withConnection ECOIN (\conn ->
        transContext conn                                             >>= \tc ->
        fetchPortfolii conn                                                  >>=
        foldM (forEachPortfolioDo conn tday tc) ([], Map.empty) . Map.elems  >>=
        pass (summarizePortfolii tday) . fst                                 >>=
        mapM_ (printPortfolioReport tday))
