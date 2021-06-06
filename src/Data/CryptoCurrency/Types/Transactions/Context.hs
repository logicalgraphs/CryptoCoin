{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.CryptoCurrency.Types.Transactions.Context where

import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Database.PostgreSQL.Simple (Connection, query, query_)
import Database.PostgreSQL.Simple.Types (Query(Query), Only(Only), In(In))

import Control.Map (snarf)

import Data.CryptoCurrency.Types.Coin (allCoinsLk, CoinIdsLookup)
import Data.CryptoCurrency.Types.Portfolio (portfoliiLk)

import Data.LookupTable

import Store.SQL.Util.LookupTable (lookupTable)
import Store.SQL.Util.Indexed (Index, idx)
import Store.SQL.Util.Pivots

-- TRANSACTION CONTEXT ---------------------------------------------------

type PortfolioAccount = Integer
type LinkedAccount = Integer

type Links = Map PortfolioAccount LinkedAccount

fetchLinksQuery :: Query
fetchLinksQuery = "SELECT portfolio_id,linked_account_id FROM j_linked_account"

fetchLinks :: Connection -> IO [Pivot]
fetchLinks = flip query_ fetchLinksQuery

data TransactionContext =
   TaC { symLk :: CoinIdsLookup, callLk, portfolioLk :: LookupTable,
         linked :: Links }
      deriving (Eq, Ord, Show)

transContext' :: Connection -> IO TransactionContext
transContext' conn =
   TaC <$> allCoinsLk conn
       <*> lookupTable conn "call_lk"
       <*> portfoliiLk conn
       <*> (Map.fromList . map toTup <$> fetchLinks conn)

{--
Okay, but what about duplicate coin-symbols? We need to eliminate those cmc_ids 
for coins of syms we already have in our portfolii. We do this by fetching the 
(symbol, cmc_id) pairs for each portfolio (from the transaction_log), then
overlaying those values on top of the symLk tac.
--}

fetchCoinsForPortfoliiQuery :: Query
fetchCoinsForPortfoliiQuery = Query . B.pack $ unlines [
   "SELECT symbol,cmc_id FROM coin WHERE cmc_id IN",
   "(SELECT distinct cmc_id FROM transaction_log WHERE portfolio_id IN ?)"]

fetchCoinsForPortfolii :: Connection -> LookupTable -> IO CoinIdsLookup
fetchCoinsForPortfolii conn (Set.fromList . Map.elems -> portIds) =
   snarf pure <$> fetchCoinsForPortfolii' conn portIds

fetchCoinsForPortfolii' :: Connection -> Set Integer -> IO [(String, Integer)]
fetchCoinsForPortfolii' conn =
   query conn fetchCoinsForPortfoliiQuery . Only . In . Set.toList

overlayPortfolioCoinIds :: Connection -> TransactionContext
                        -> IO TransactionContext
overlayPortfolioCoinIds conn tac@(TaC sy _ ports _) =
   fetchCoinsForPortfolii conn ports >>= \portCoins ->
   return (tac { symLk = Map.union portCoins sy })

-- So that means transContext now becomes:

transContext :: Connection -> IO TransactionContext
transContext conn = transContext' conn >>= overlayPortfolioCoinIds conn

{--
Proof? COMP and UNI should be reduced.

>>> import Database.PostgreSQL.Simple
>>> import Store.SQL.Connection 
>>> conn <- connectInfo ECOIN >>= connect
>>> tc0 <- transContext' conn
>>> Map.lookup "COMP" (symLk tc0)
Just (fromList [3180,5692])
>>> Map.lookup "UNI" (symLk tc0)
Just (fromList [1605,4307,7083])

... and then:

>>> tc1 <- transContext conn
>>> Map.lookup "COMP" (symLk tc1)
Just (fromList [5692])
>>> Map.lookup "UNI" (symLk tc1)
Just (fromList [7083])

TADA! Visual inspection confirms the cmc_ids are the ones in the GEMINI 
portfolio.
--}
