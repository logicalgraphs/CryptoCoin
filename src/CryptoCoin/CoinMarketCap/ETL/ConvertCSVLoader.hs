{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.ETL.ConvertCSVLoader where

{-- 
Read in a CSV file directing a coin conversion from, eg: DOGE to MIR

The conversion file format is as follows:

date,portfolio,from,amt,quote,to,amt,quote,coin fee,commission,tax,confirm
2021-07-15,coinbase,1INCH,30.91419707,MANA,98.80691669,0.0,0.0,0.0
2021-07-15,coinbase,COMP,1.43180352,MANA,845.1539316,0.0,0.0,0.0
--}

import Data.Char (toUpper)

import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection)

import Control.Scan.CSV (readMaybe)

import CryptoCoin.CoinMarketCap.Utils (geaux, dateDir)

import Data.CryptoCurrency.Types.Conversion (ConvertData(CD), Spot(Spot))
import Data.CryptoCurrency.Utils (report, plural, fileProcessor)

import Data.XHTML (Name)

sampleConvertCoin :: [String]
sampleConvertCoin =
   ["2021-07-15,coinbase,1INCH,30.91419707,MANA,98.80691669,0.0,0.0,0.0",
    "2021-07-15,coinbase,COMP,1.43180352,MANA,845.1539316,0.0,0.0,0.0,xyzabc"]

snarfConversion :: [String] -> Maybe ConvertData
snarfConversion a@[_,_,_,_,_,_,_,_,_] = sC1 a Nothing
snarfConversion a@[_,_,_,_,_,_,_,_,_,conf] = sC1 (init a) (Just conf)
snarfConversion _ = Nothing

sC1 :: [String] -> Maybe Name -> Maybe ConvertData
sC1 [dt,port,c0,amt0,c1,amt1,fee,comm,tax] confirm =
   readMaybe dt   >>= \dtz ->
   readMaybe amt0 >>= \amt0z ->
   readMaybe amt1 >>= \amt1z ->
   readMaybe fee  >>= \feez ->
   readMaybe comm >>= \commz ->
   readMaybe tax  >>= \txz   ->
   return (CD dtz port (Spot c0 amt0z _needQuote) (Spot c1 amt1z _needQ2) feez commz txz confirm)

{--
>>> snarfConversion (head sampleConvertCoin)
Just (CD 2021-07-15 "coinbase" "1INCH" 30.91419707 "MANA" 98.80691669 0.0 0.0 
      0.0 Nothing)

>>> snarfConversion (last sampleConvertCoin)
Just (CD 2021-07-15 "coinbase" "COMP" 1.43180352 "MANA" 845.1539316 0.0 0.0 
      0.0 (Just "xyzabc"))

Now let's load a set of conversion into the database.
--}

go :: IO ()
go = geaux convertCoins

convertCoins :: Connection -> Day -> IO ()
convertCoins conn tday =
   dateDir conn "conversions" >>=
   fileProcessor snarfConversion . (++ "convert.csv") >>= \conv ->
   report 0 (msg (length conv))
            (transContext conn >>= flip (storeConvertData conn) conv)

msg :: Int -> String
msg x = "Storing " ++ show x ++ " coin conversion" ++ plural x
