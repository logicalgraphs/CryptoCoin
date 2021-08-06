{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.Reports.Portfolio where

{--
Gives a report of the coins and the balances and the totals of each portfolio.

Woo, boy!

1.1: First do the transactions (DONE), then add and subtract each coin transfer
     from-portfolio = coins - amt xfered
     to-portfolio   = coins + (amt - surcharge) xfered

To incorporate the coin transfer (really, exchange transfer), we need to 
restructure the PorfolioReport-type:

Map Name PortfolioReport  (portfolioName -> PortfolioReport)

And the holdings type from Set Holding to

Map Idx Holding (coinId -> Holding)
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
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection)

import Control.Logic.Frege ((-|))
import Control.Presentation hiding (S)
import CryptoCoin.CoinMarketCap.Reports.Table hiding (ecoin)
import CryptoCoin.CoinMarketCap.Types
       (Listings, ECoin, fetchListings, coin, quote)
import CryptoCoin.CoinMarketCap.Types.Quote (price)

import Data.CryptoCurrency.Types
       (Indexed, Rank, rank, Idx, idx, namei, sym, IxRow(IxRow))
import Data.CryptoCurrency.Types.Coin (allCoinsLk)
import Data.CryptoCurrency.Types.Portfolio
       (Portfolio(Portfolio), fetchPortfolii, portfolioName, fetchCoinIdsFor,
        cash, Portfolii)
import Data.CryptoCurrency.Types.Recommendation (Call(BUY), Recs, fetchRecs)
import Data.CryptoCurrency.Types.Transaction
       (Transaction(Transaction), fetchTransactionsByPortfolio, spent, ncoins)
import Data.CryptoCurrency.Types.Transactions.Context
       (TransactionContext, transContext)
import Data.CryptoCurrency.Types.Transfers.Coin
       (fetchCoinTransfers, CoinTransferDatum(CoinTransferDatum), CoinTransfer)
import Data.CryptoCurrency.Utils (pass)
import Data.LookupTable (LookupTable)
import Data.Monetary.USD (USD(USD), doubledown)
import Data.Percentage (Percentage(P))
import Data.XHTML (Name)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed (IxValue(IxV))
import Store.SQL.Util.LookupTable (lookupTable, lookupTableFrom)
import Store.SQL.Util.Time (latest)

data Holding =
   Holding { ecoin :: ECoin, amount :: Double, invested, currentPrice :: USD }
      deriving (Eq, Ord, Show)

data HoldRec = HR { holding :: Holding, call :: Maybe Call }
   deriving (Eq, Ord, Show)

instance Indexed Holding where
   idx = idx . ecoin
instance Indexed HoldRec where
   idx = idx . holding

instance Rank Holding where
   rank = rank . ecoin
instance Rank HoldRec where
   rank = rank . holding

instance Univ Holding where
   explode h@(Holding e amt ui@(USD inv) up@(USD pric)) =
      let tramt = toRational amt
          currVal = tramt * toRational pric
          ave = USD (inv / tramt) in
      [show (rank h), show (idx h), sym e, namei e, show amt, show ui, show up,
       show ave, show (USD currVal), show (change inv currVal)]

instance Univ HoldRec where
   explode (HR h c) = explode h ++ mbshow c
      where mbshow Nothing = []
            mbshow (Just c) = [show c]

data PortfolioReport =
   PR { portfolio :: Portfolio, holdings :: Map Idx HoldRec }
      deriving (Eq, Ord, Show)

instance Monoid PortfolioReport where
   mempty = PR (Portfolio "Nada" (USD 0) Nothing) Map.empty
   PR p0 h0 `mappend` PR p1 h1 =
       PR (Portfolio "All Portfolii" (cash p0 + cash p1) Nothing)
          (Map.union h0 h1)

totalInvested, totalValue :: PortfolioReport -> USD
totalInvested (PR _ holds) = sumOver (invested . holding) holds
totalValue (PR _ holds) =
   let val h = USD (toRational (amount h * doubledown (currentPrice h))) in
   sumOver (val . holding) holds

toHolding :: Foldable t => Recs -> Listings -> Idx -> t Transaction
          -> Maybe HoldRec
toHolding r listings ix transes = 
   Map.lookup ix listings                           >>= \listing ->
   quote listing                                    >>= \quot ->
   buildHolding (coin listing) (price quot) transes >>=
   return . flip HR (Map.lookup ix r)

buildHolding :: Foldable t => ECoin -> Double -> t Transaction -> Maybe Holding
buildHolding coin pric transes =
   let coins    = sumOver ncoins transes
       invested = sumOver spent transes
       ur = USD . toRational
       currPrice = ur pric
       stake = ur (coins * pric)
   in  stake > USD 1 -| Holding coin coins invested currPrice

sumOver :: Foldable t => Num b => (a -> b) -> t a -> b
sumOver f = sum . map f . toList

-- Now we build the table/rasa-representation of the portfolio

data KV = KV String String
   deriving (Eq, Ord, Show)

instance Univ KV where explode (KV k v) = [k, v]

portHdrs :: [String]
portHdrs =
   words "Rank Id Symbol Coin Amount Invested Price Ave Value %change call"

portTitle :: Portfolio -> String
portTitle p = "Holdings for " ++ portfolioName p ++ " portfolio"

printPortfolioCSV :: Day -> PortfolioReport -> IO ()
printPortfolioCSV date pr@(PR p holdings) =
   let cr = putStrLn "" in
   cr                         >>
   summarizePortfolio date pr >>
   if holdings == Map.empty then return ()
   else cr                    >>
   csvReport' portHdrs (portTitle p) (sortOn rank (Map.elems holdings))

data PorNot = Perc Percentage | PNOT
   deriving Eq

change :: Rational -> Rational -> PorNot
change inv val = if inv < 1 then PNOT else Perc (P ((val - inv) / inv))

instance Show PorNot where
   show (Perc p) = show p
   show PNOT = "N/A"
   
forEachPortfolioDo :: Connection -> Day -> TransactionContext -> Recs
                   -> ([PortfolioReport], Listings) -> IxValue Portfolio
                   -> IO ([PortfolioReport], Listings)
forEachPortfolioDo conn tday tc recs (prs, lists) i@(IxV ix port) =
   fetchCoinIdsFor conn i                                     >>= \coinIds ->
   let newIds = Set.difference coinIds (Map.keysSet lists)
       portName = portfolioName port in
   fetchListings conn tday newIds                             >>= \newLists ->
   fetchTransactionsByPortfolio conn tc portName              >>= \transs ->
   let combinedListings = Map.union newLists lists
       coinsHeld = mapMaybe (uncurry (toHolding recs combinedListings))
                            (Map.toList transs)
   in  return (PR port (Map.fromList $ map (idx &&& id) coinsHeld):prs,
               combinedListings)

{--
Now let's roll in the coin-transfers. Now, we have to be smart about this,
because we have the coin-transfers, but we also have the coin-conversions,
both of which are date-sensitive. That means we have to go to the coin-
conversion first, get the first date, then execute all the transfers that 
happen before that date, THEN execute the coin-conversion, lather, rinse, 
repeat until we run out of coin-conversions, then finish off the remaining
coin-transfers.

Let's see what the coin-transfers look like:

>>> import Database.PostgreSQL.Simple
>>> import Store.SQL.Connection 
>>> conn <- connectInfo ECOIN >>= connect
>>> pLk <- portfoliiLk conn
>>> xfers <- fetchCoinTransfers conn pLk
>>> length xfers
40
>>> take 2 xfers
[IxRow 5692 2021-07-06
       (CoinTransferDatum {amount = 0.12445026, surcharge = 7.3138e-3,
                           basis = $0.00, xfrom = "COINBASE", xto = "GEMINI"}),
 IxRow 1 2021-06-18 (CoinTransferDatum {amount = 2.83418e-3, 
                                        surcharge = 1.025e-5, basis = $100.00,
                                        xfrom = "COINBASE", xto = "PHEMEX"})]

lolneet! ... but this also means we want to sort these pupperz by date: DONE!

[IxRow 1 2021-06-18
         (CoinTransferDatum {amount = 3.66589e-3, surcharge = 5.7e-4,
                             basis = $150.00, 
                             xfrom = "PHEMEX", xto = "COINBASE"})
 IxRow 1 2021-06-18
         (CoinTransferDatum {amount = 2.83418e-3, surcharge = 1.025e-5,
                             basis = $100.00,
                             xfrom = "COINBASE", xto = "PHEMEX"})]

Voilà. Now we know what to do, ... at least for the coin-transfer-part.
--}

type PortfolioReports = Map Name PortfolioReport

transferCoins :: Connection -> Portfolii -> Recs
              -> ([PortfolioReport], Listings) -> IO PortfolioReports
transferCoins conn (Map.map idx -> pLk) recs (reports, listings) =
   let toMap = Map.fromList . map (portfolioName . portfolio &&& id) in
   putStrLn "Transferring coins between portfolii" >>
   fetchCoinTransfers conn pLk                     >>=
   foldM (transferCoin listings recs) (toMap reports)

data Haben = REQUIRED | OPTIONAL
   deriving Eq

transferCoin :: Listings -> Recs -> PortfolioReports -> CoinTransfer
             -> IO PortfolioReports
transferCoin l r m x@(IxRow cid _dt (CoinTransferDatum amt sur bas fr xto)) =
   maybe (putStrLn ("***ERROR! Could not record transfer " ++ show x) >>
          return m)
         return
         (adjustHolding m fr l r cid (-1 * amt) (-1 * bas) REQUIRED >>= \m1 ->
          adjustHolding m1 xto l r cid (amt - sur) bas OPTIONAL)

adjustHolding :: PortfolioReports -> Name -> Listings -> Recs -> Idx -> Double
              -> USD -> Haben -> Maybe PortfolioReports
adjustHolding reps port = ah' port reps (Map.lookup port reps)

ah' :: Name -> PortfolioReports -> Maybe PortfolioReport -> Listings -> Recs
    -> Idx -> Double -> USD -> Haben -> Maybe PortfolioReports
ah' n _ Nothing _ _ _ _ _ _ = error ("Could not find portfolio named " ++ n)
ah' port reps (Just pr) listings recs cid amt bas hab =
   let hs = holdings pr in
   ah'' listings recs port reps pr hs (Map.lookup cid hs) cid amt bas hab

ah'' :: Listings -> Recs -> Name -> PortfolioReports -> PortfolioReport
     -> Map Idx HoldRec -> Maybe HoldRec -> Idx -> Double -> USD -> Haben
     -> Maybe PortfolioReports
ah'' _ _ port _ _ _ Nothing cid _ _ REQUIRED =
   -- error ("No holdings for coin #" ++ show cid ++ " for portfolio " ++ port)
   Nothing
ah'' listings recs port reps pr hs Nothing cid amt bas OPTIONAL =
   let transs = [Transaction "" undefined bas undefined amt BUY ""] in
   updateHolding port reps pr hs cid <$> toHolding recs listings cid transs
ah'' _ _ port reps pr hs (Just hr@(HR h@(Holding _ am inv _) _)) cid amt bas _ =
   let newholding = h { amount = am + amt, invested = inv + bas } in
   Just (updateHolding port reps pr hs cid (hr { holding = newholding }))

updateHolding :: Name -> PortfolioReports -> PortfolioReport -> Map Idx HoldRec
              -> Idx -> HoldRec -> PortfolioReports
updateHolding port reps pr hs cid newhold =
   Map.insert port (pr { holdings = Map.insert cid newhold hs }) reps

cleanPortfolii :: PortfolioReports -> [PortfolioReport]
cleanPortfolii = map cleanPortfolio . Map.elems

cleanPortfolio :: PortfolioReport -> PortfolioReport
cleanPortfolio pr = let hs = holdings pr in pr { holdings = cleanHoldings hs }

cleanHoldings :: Map Idx HoldRec -> Map Idx HoldRec
cleanHoldings = Map.filter ((> 0) . amount . holding)

buildSummary :: PortfolioReport -> [KV]
buildSummary pr@(PR (Portfolio name reserve _) holdings) =
   let ti@(USD totInv) = totalInvested pr
       tv@(USD totVal) = totalValue pr in
   [KV "Cash reserve" (show reserve)]
   ++ if holdings == Map.empty then []
      else [KV "Total invested" (show ti),
            KV "Current value" (show tv),
            KV "Gain/Loss" (show $ change totInv totVal)]

summarizePortfolio :: Day -> PortfolioReport -> IO ()
summarizePortfolio date pr@(PR (Portfolio name _ _) holdings) =
   csvReport' [] (unwords [show date ++ ",Summary of", name]) (buildSummary pr)

summarizePortfolii :: Day -> [PortfolioReport] -> IO ()
summarizePortfolii date = summarizePortfolio date . mconcat

realizePortfolioReports :: Connection -> Day -> Portfolii -> Recs
                        -> IO ([PortfolioReport], Listings)
realizePortfolioReports conn date ports r =
   transContext conn   >>= \tc ->
   foldM (forEachPortfolioDo conn date tc r) ([], Map.empty) (Map.elems ports)

go :: IO ()
go = withConnection ECOIN (\conn ->
        latest conn "coin_market_cap_daily_listing" "for_date"     >>= \tday ->
        fetchPortfolii conn                                        >>= \ports ->
        fetchRecs conn tday                                        >>= \recs ->
        realizePortfolioReports conn tday ports recs               >>=
        transferCoins conn ports recs                              >>=
        pass (summarizePortfolii tday) . cleanPortfolii            >>=
        mapM_ (printPortfolioCSV tday))
