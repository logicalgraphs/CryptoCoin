{-# LANGUAGE OverloadedStrings #-}

module Data.CryptoCurrency.Types.Transactions.Context where

import Data.Map (Map)
import qualified Data.Map as Map

import Database.PostgreSQL.Simple (Connection, Query, query_)

import Data.CryptoCurrency.Types.Coin (allCoinsLk, CoinIdsLookup)
import Data.CryptoCurrency.Types.Portfolio (portfoliiLk)

import Data.LookupTable

import Store.SQL.Util.LookupTable (lookupTable)
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

transContext :: Connection -> IO TransactionContext
transContext conn =
   TaC <$> allCoinsLk conn
       <*> lookupTable conn "call_lk"
       <*> portfoliiLk conn
       <*> (Map.fromList . map toTup <$> fetchLinks conn)
