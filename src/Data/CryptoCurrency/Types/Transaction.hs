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

import Control.Map (snarf)
import Control.Scan.CSV (readMaybe)

import Data.CryptoCurrency.Types
import Data.CryptoCurrency.Types.Recommendation
import Data.CryptoCurrency.Types.Transactions.Internal
import Data.LookupTable
import Data.Monetary.USD

import Store.SQL.Util.Pivots (Pivot(Pvt))

data Transaction = Transaction Symbol Day USD USD Double Call String
   deriving (Eq, Ord, Show)

-- STORE FUNCTIONS -------------------------------------------------------

toTrans' :: LookupTable -> LookupTable -> LookupTable -> Transaction
         -> Maybe Trans'
toTrans' coinLk callLk portfolioLk (Transaction cn dt xn sch cns cl port) =
   Trans' dt xn sch cns <$> lk (show cl) callLk
                        <*> lk port portfolioLk
                        <*> lk cn coinLk
      where lk = Map.lookup

storeTransaction :: Connection -> LookupTable -> LookupTable -> LookupTable
                 -> Transaction -> IO (Maybe CoinDayTransaction)
storeTransaction conn coinLk callLk portLk =
   maybe (pure Nothing) (\t -> Just <$> realize conn t)
       . toTrans' coinLk callLk portLk

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
pvtCoin2Trans c2t (Pvt i j) = flip Pvt j <$> Map.lookup i c2t

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

type CoinTransactions = Map Idx (Set Transaction)

fetchTransactions :: Connection -> LookupTable -> LookupTable -> LookupTable
                  -> String -> IO CoinTransactions
fetchTransactions conn symLk callLk portLk portfolio =
   let whereClause = ("WHERE portfolio_id=" ++) . show
                     <$> Map.lookup portfolio portLk
       symld = lookdown symLk
       callld = lookdown callLk
       portld = lookdown portLk
   in  maybe (return Map.empty) 
             (\wc -> snarf (sequence . (idx &&& fromTrans' symld callld portld))
                           <$> fetchTrans conn wc)
             whereClause
