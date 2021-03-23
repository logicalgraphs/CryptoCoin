{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Types.Internal where

import Data.Aeson

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Map (Map)

import System.Environment (getEnv)

import Data.CryptoCurrency.Types   -- for indexed
import CryptoCoin.CoinMarketCap.Types.Quote

{--
data Coin' = Coin' { id :: Idx, name, symbol, slug :: String,
                     rank' :: Int,
                     date_added :: String,
                     platform :: Maybe CoinRef' }
   deriving (Eq, Ord, Show)

instance FromJSON Coin' where
   parseJSON = withObject "Raw coin" $ \v ->
      Coin' <$> v .: "id"
            <*> v .: "name"
            <*> v .: "symbol"
            <*> v .: "slug"
            <*> v .: "cmc_rank"
            <*> v .: "date_added",
            <*> v .:? "platform"
--}

type TokenAddress = String

data CoinRef' = CR' Idx TokenAddress
   deriving (Eq, Ord, Show)

instance Indexed CoinRef' where
   idx (CR' i _) = i

instance FromJSON CoinRef' where
   parseJSON = withObject "ref" $ \v ->
      CR' <$> v .: "id" <*> v .: "token_address"

data Listing' =
   Listing' Integer String String String
            Integer String Double Double (Maybe Double) [String]
            (Maybe CoinRef') Integer
            (Map String Quote)
      deriving Show

plat :: Listing' -> Maybe CoinRef'
plat (Listing' _ _ _ _ _ _ _ _ _ _ p _ _) = p

instance FromJSON Listing' where
   parseJSON = withObject "listing" $ \v ->
      Listing' <$> v .: "id" 
               <*> v .: "name"
               <*> v .: "symbol"
               <*> v .: "slug"
               <*> v .: "num_market_pairs" 
               <*> v .: "date_added"
               <*> v .: "circulating_supply"
               <*> v .: "total_supply"
               <*> v .:? "max_supply"
               <*> v .: "tags"
               <*> v .:? "platform"
               <*> v .: "cmc_rank"
               <*> v .: "quote"

sample :: String -> IO ByteString
sample thing =
   getEnv "COIN_MARKET_CAP_DIR" >>=
   BL.readFile . (++ "/ETL/sample" ++ thing ++ ".json")
