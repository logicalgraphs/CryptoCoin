{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.SourceFileLoader where

{--
Takes the JSON we downloaded from crypto-coin web APIs and loads those files
to our SQL data-store.
--}

import qualified Data.ByteString.Char8 as B

import Data.Char (toLower)

import Data.List (isSuffixOf)
import qualified Data.Map as Map

import Data.Time

import System.Directory
import System.Environment (getEnv)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Control.Presentation
import Control.Scan.CSV

import Data.LookupTable
import Data.Time.TimeSeries (today)

import Store.SQL.Connection
import Store.SQL.Util.LookupTable

uploadFileQuery :: Query
uploadFileQuery = Query . B.pack $ unwords [
   "INSERT INTO source (source_type_id, file_name, for_day, file)",
   "VALUES (?, ?, ?, ?) RETURNING source_id"]

uploadFile :: Integer -> FilePath -> Connection -> IO (FilePath, Integer)
uploadFile sourceType filename conn =
   readFile filename                        >>= \file ->
   today                                    >>= \tday ->
   query conn uploadFileQuery
         (sourceType, filename, tday, file) >>= \[Only i] ->
   putStrLn ("Uploaded " ++ filename)       >>
   removeFile filename                      >>
   putStrLn ("Removed file " ++ filename)   >>
   return (filename, i)

uploadAllFilesAt :: FilePath -> Integer -> Connection -> IO [(FilePath, Integer)]
uploadAllFilesAt dir srcTyp conn =
   listDirectory dir       >>= \files ->
   setCurrentDirectory dir >>
   mapM (flip (uploadFile srcTyp) conn) (filter (".json" `isSuffixOf`) files)

{--
>>> conn <- connection >>= connect
>>> src <- lookupTable conn "source_type_lk"
>>> src
fromList [("FCAS",3),("LISTING",2),("RANKING",1)]
>>> cmcDir <- getEnv "COIN_MARKET_CAP_DIR"
>>> uploadAllFilesAt (cmcDir ++ "/listings") (src Map.! "LISTING") conn
Uploaded listings-2021-03-05.json
[("listings-2021-03-05.json",16)]
>>> close conn
--}

go :: IO ()
go = withConnection ECOIN (\conn ->
   lookupTable conn "source_type_lk" >>= \src ->
   getEnv "COIN_MARKET_CAP_DIR"      >>= \cmcDir ->
   let uploader dir typ = uploadAllFilesAt (cmcDir ++ ('/':dir))
                                           (src Map.! typ) conn
   in  uploader "listings" "LISTING" >>
       uploader "scores"   "FCAS"    >>
       sources conn)

{--
>>> go
Uploaded listings-2021-03-09.json
--}

data Source = Source { idx :: Integer, source :: String, forDay :: Day,
                       file :: String, processed :: Bool }
   deriving (Eq, Show)

instance FromRow Source where
   fromRow = Source <$> field <*> field <*> field <*> field <*> field

instance Univ Source where
   explode (Source i s d f p) = [show i, s, show d, f, showBool p]

showBool :: Bool -> String
showBool = (flip (:) . tail <*> toLower . head) . show

-- SQL query to check that the database is populated:

sourceQuery :: Day -> Query
sourceQuery tday = Query . B.pack $ unlines [
   "SELECT a.source_id, b.source_type, a.for_day, a.file_name, a.processed",
   "FROM source a",
   "INNER JOIN source_type_lk b ON b.source_type_id=a.source_type_id",
   concat ["WHERE a.for_day > '", show tday, "'"],
   "ORDER BY a.file_name DESC"]

srcs :: Connection -> Day -> IO [Source]
srcs conn day = 
   let srcQuery = sourceQuery day in
   B.putStrLn (fromQuery srcQuery) >> query_ conn srcQuery

sources :: Connection -> IO ()
sources conn = getCurrentTime                     >>=
               srcs conn . addDays (-5) . utctDay >>=
               csvHeader                          >>=
               mapM_ (putStrLn . uncsv)
  where csvHeader s = putStrLn "id,source_type,for_day,file_name,processed" >>
                      return s
