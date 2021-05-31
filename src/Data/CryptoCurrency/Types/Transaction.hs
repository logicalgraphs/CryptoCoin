{-# LANGUAGE OverloadedStrings #-}

module Data.CryptoCurrency.Types.Transaction where

{--
A transaction HAS_A

   call
   portfolio (id) (that we get from the portfolio name and portfolio map)
     (so we need a portfolio map)
   coin id we get from the coin symbol

   the rest we get from the transaction details

Now we need the transaction Id to associate to the recommendations for the
coin at this date bought or sold in this transaction.
--}

import Control.Arrow ((&&&))
import Control.Monad (join)

import qualified Data.ByteString.Char8 as B

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (Day)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import Control.Map (snarfL)
import Control.Scan.CSV (readMaybe)

import Data.CryptoCurrency.Types
import Data.CryptoCurrency.Types.Coin (lookdownCoinSyms)
import Data.CryptoCurrency.Types.Recommendation
import Data.CryptoCurrency.Types.Transactions.Internal
import Data.CryptoCurrency.Types.Transactions.Context
import Data.LookupTable
import Data.Monetary.USD

import Store.SQL.Util.LookupTable
import Store.SQL.Util.Pivots (Pivot(Pvt))

data Transaction = Transaction Symbol Day USD USD Double Call String
   deriving (Eq, Ord, Show)

ncoins :: Transaction -> Double
ncoins (Transaction _ _ _ _ n _ _) = n

spent :: Transaction -> USD
spent (Transaction _ _ s _ _ _ _) = s

-- STORE FUNCTIONS -------------------------------------------------------

toTrans' :: TransactionContext -> Transaction -> Maybe Trans'
        
toTrans' (TC coinLk callLk portfolioLk) (Transaction cn dt xn sch cns cl port) =
   Trans' dt xn sch cns <$> lk (show cl) callLk
                        <*> lk port portfolioLk
                        <*> (head . Set.toList <$> lk cn coinLk)
      where lk = Map.lookup

storeTransaction :: Connection -> TransactionContext -> Transaction
                 -> IO (Maybe CoinDayTransaction)
storeTransaction conn ctx = 
   maybe (pure Nothing) (\t -> Just <$> realize conn t) . toTrans' ctx

gather :: Foldable t => Ord b => (a -> b) -> t a -> Set b
gather f = Set.fromList . map f . toList

pvt :: Query
pvt = Query . B.pack $ unlines [
   "INSERT INTO j_transaction_recommendation (transaction_id,",
   "recommendation_id) VALUES (?, ?)"]

-- joinRecs has the gaping hole-across-days, so we should focus on using
-- this function within the span of no more than one day

joinRecommendations :: Foldable t => Connection -> t CoinDayTransaction -> IO ()
joinRecommendations conn cdts = 
   let dates = gather date cdts
       coinIds = gather idx cdts
       c2t = Map.fromList (map (idx &&& row) (toList cdts))
   in  fetchCoinRecs conn dates coinIds                     >>=
       executeMany conn pvt . mapMaybe (pvtCoin2Trans c2t)  >>=
       putStrLn . ("Joined " ++) . (++ " recommendations to transactions.")
                . show

pvtCoin2Trans :: Map Integer Integer -> Pivot -> Maybe Pivot
pvtCoin2Trans c2t (Pvt coinId recId) = flip Pvt recId <$> Map.lookup coinId c2t

-- FETCH FUNCTIONS -------------------------------------------------------

type LookDownS = LookDown String

fromTrans' :: LookDownS -> LookDownS -> LookDownS -> Trans' -> Maybe Transaction
fromTrans' symLk callLk portLk (Trans' dt amt sur cns callId portId symId) =
   let lk = Map.lookup in
   lk symId symLk >>= \sym ->
   Transaction sym dt amt sur cns <$> join (readMaybe <$> lk callId callLk)
                                  <*> lk portId portLk

instance Indexed Trans' where
   idx (Trans' _ _ _ _ _ _ symId) = symId

type CoinTransactions = Map Idx [Transaction]

fetchTransactionsByPortfolio :: Connection -> TransactionContext -> String 
                             -> IO CoinTransactions
fetchTransactionsByPortfolio conn tc@(TC _ _ portLk) portfolio =
   let whereClause = ("WHERE portfolio_id=" ++) . show
                     <$> Map.lookup portfolio portLk
   in  maybe (return Map.empty) (doFetchTransaction conn tc) whereClause

-- this also breaks out the transactions by not-recommended and recommended

fetchTransactionsByDate :: Connection -> TransactionContext -> Day 
                       -> IO (CoinTransactions, CoinTransactions)
fetchTransactionsByDate conn tc date =
   let whereClause = "WHERE for_date='" ++ show date ++ "'" in
   doFetchTransaction conn tc whereClause >>= \transmap ->
   fetchCoinRecs conn [date] (Map.keys transmap) >>=
   return . flip Map.partitionWithKey transmap . (const .) . idIn . map coinId
      where coinId (Pvt cid _) = cid
            idIn = flip Set.member . Set.fromList

doFetchTransaction :: Connection -> TransactionContext -> String
                   -> IO CoinTransactions
doFetchTransaction conn (TC symLk callLk portLk) whereClause =
   let symld = lookdownCoinSyms symLk
       callld = lookdown callLk
       portld = lookdown portLk
       ixTransF = idx &&& fromTrans' symld callld portld
   in  snarfL (sequence . ixTransF) <$> fetchTrans conn whereClause 
