{-# LANGUAGE OverloadedStrings #-}

module Data.CryptoCurrency.Types.Portfolio where

{--
A portfolio (value) is a relatively-simple type that collects the e-coin
transactions under this umbrella.
--}

import Control.Arrow ((&&&))

import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Data.LookupTable
import Data.Monetary.USD

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed (IxValue(IxV), val, Index, idx)
import Store.SQL.Util.LookupTable (lookupTableFrom)

data Portfolio =
   Portfolio { portfolioName :: String, cash :: USD, raison :: Maybe String }
      deriving (Eq, Ord, Show)

-- that's it. That's the tweet, lol.

-- Now, we can (programatically) create portfolii, but we don't need to do
-- that until we have a mess-a custumuhz, so, for now we'll just read'm
-- from the database (and add'm to the database when we want new ones, see?)

instance FromRow Portfolio where
   fromRow = Portfolio <$> field <*> field <*> field

fetchPortfolii :: Connection -> IO (Map String (IxValue Portfolio))
fetchPortfolii conn =
   Map.fromList . map (portfolioName . val &&& id)
      <$> query_ conn fetchPortfoliiQuery

fetchPortfoliiQuery :: Query
fetchPortfoliiQuery = Query . B.pack $ unlines [
   "SELECT p.portfolio_id, p.portfolio_name, p.cash, ttlk.tracked_type",
   "FROM portfolio p",
   "INNER JOIN tracked_type_lk ttlk ON ttlk.tracked_type_id=p.tracked_type_id"]

fetchCoinsQuery :: Integer -> Query
fetchCoinsQuery portId = Query $ B.pack
   ("SELECT DISTINCT cmc_id FROM transaction_log WHERE portfolio_id="
     ++ show portId)

fetchCoinIdsFor' :: Connection -> IxValue a -> IO (Set Index)
fetchCoinIdsFor' conn (IxV ix _) =
   Set.fromList <$> query_ conn (fetchCoinsQuery ix)

fetchCoinIdsFor :: Connection -> IxValue a -> IO (Set Integer)
fetchCoinIdsFor conn ix = Set.map idx <$> fetchCoinIdsFor' conn ix

{--
>>> withConnection ECOIN (\conn -> fetchPortfolii conn >>=
          mapM_ (\port -> sequence (port, fetchCoinIdsFor conn port) >>= print)
                 . Map.elems)
(IxV {ix = 2, val = Portfolio {portfolioName = "BINANCE", cash = $0.00,
                               raison = Just "BINANCE"}},[])
(IxV {ix = 1, val = Portfolio {portfolioName = "COINBASE", cash = $0.00, 
                               raison = Just "COINBASE"}},
 fromList [1,512,4761,5692,6719,9421])
--}

-- for a lookup-table:

portfoliiLk :: Connection -> IO LookupTable
portfoliiLk conn =
   lookupTableFrom conn "SELECT portfolio_id, portfolio_name FROM portfolio"
