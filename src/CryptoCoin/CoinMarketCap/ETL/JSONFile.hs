{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.JSONFile where

-- types shared across the ETL codebase.

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Either (rights)

import qualified Data.Map as Map

import Data.LookupTable

import CryptoCoin.CoinMarketCap.Types

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.LookupTable

extractJSONQuery :: Query
extractJSONQuery =
   "SELECT source_id, file FROM SOURCE WHERE processed=? AND source_type_id=?"

data JSONFile = JSONFile { fileId :: Integer, file :: String }
   deriving (Eq, Show)

instance FromRow JSONFile where
   fromRow = JSONFile <$> field <*> field

extractFile :: String -> Connection -> LookupTable -> IO [JSONFile]
extractFile lk conn srcs =
   query conn extractJSONQuery (False, srcs Map.! lk)

type ValOrErr a = Either String a
type IxValOrErr = ValOrErr (IxValue MetaData)

extractJSON :: String -> Connection -> LookupTable -> IO [IxValOrErr]
extractJSON lk conn srcs =
   extractFile lk conn srcs >>=
   mapM (\(JSONFile i f) -> pure (IxV i <$> eitherDecode (BL.pack f)))

extractJSON' :: String -> Connection -> LookupTable -> IO [(Int, IxValOrErr)]
extractJSON' lk conn srcs = zip [1..] <$> extractJSON lk conn srcs

extractListings :: Connection -> LookupTable -> IO [IxValue MetaData]
extractListings conn lk = rights . map snd <$> extractJSON' "LISTING" conn lk

{--
>>> withConnection ECOIN (\conn -> lookupTable conn "source_type_lk" >>=
                                   extractListings conn              >>=
                                   mapM_ (\(IxV i (MetaData s _)) -> print (i,s)))
(41,Status 2021-03-20 0 Nothing 752 23 Nothing)
(44,Status 2021-03-21 0 Nothing 271 23 Nothing)
(45,Status 2021-03-22 0 Nothing 197 23 Nothing)

... returns only the non-processed listings-files.

Okay, we're extracting and translating the JSON. Now we have to load these
data into the tables... somehow.

Also, note the dates. We have to correct them to local dates before we upload
data twice for one day.

TODO!
--}

try :: Show a => (Connection -> LookupTable -> IO [a]) -> IO ()
try proc = withConnection ECOIN (\conn ->
   lookupTable conn "source_type_lk" >>=
   proc conn                         >>=
   mapM_ print)
