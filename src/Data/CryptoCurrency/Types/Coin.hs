{-# LANGUAGE OverloadedStrings #-}

module Data.CryptoCurrency.Types.Coin where

import Control.Arrow ((&&&))

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

import Database.PostgreSQL.Simple (Connection, query_)

import Control.Map (snarf)
import Data.CryptoCurrency.Types (Symbol)
import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed (Index, IxValue, ix2tup, idx, val)
import Store.SQL.Util.TaggedTypes (TaggedType, untag)

type CoinIdsLookup = Map Symbol (Set Integer)

allCoinsLk :: Connection -> IO CoinIdsLookup
allCoinsLk conn = snarf (pure . (untag . val &&& idx)) <$> allCoinsList conn

allCoinsList :: Connection -> IO [IxValue (TaggedType Symbol)]
allCoinsList conn = query_ conn "SELECT cmc_id, symbol FROM coin"

lookdownCoinSyms :: CoinIdsLookup -> Map Integer Symbol
lookdownCoinSyms =
   Map.fromList . map swap . (>>= traverse Set.toList) . Map.toList

allCoinIds :: Connection -> IO (Set Integer)
allCoinIds conn = Set.fromList . map idx <$> allCoinIds' conn

allCoinIds' :: Connection -> IO [Index]
allCoinIds' conn = query_ conn "SELECT DISTINCT cmc_id FROM coin"

{--
>>> withConnection ECOIN (\conn -> allCoinIds conn >>= print . take 5 . Set.toList)
[1,2,3,4,5]

>>> withConnection ECOIN (\conn -> allCoinsLk conn >>= print . take 5 . Map.toList)
[("$ANRX",fromList [8057]),("$BASED",fromList [6570]),("$COIN",fromList [7796]),
 ("$KEI",fromList [10048]),("$KING",fromList [7569])]

>>> withConnection ECOIN (\conn -> allCoinsLk conn >>= print . take 5
                                     . Map.toList . Map.filter ((> 1) . length))
[("AAPL",fromList [7894,7924]),("ACE",fromList [2311,5717,9792]),
 ("ACM",fromList [3386,8538]),("ACOIN",fromList [601,5465]),
 ("ACU",fromList [4260,7167])]
--}
