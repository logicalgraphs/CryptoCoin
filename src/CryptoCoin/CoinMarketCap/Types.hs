{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Types where

import Control.Arrow ((&&&))

import Data.Aeson

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Time

import Data.CryptoCurrency.Types
import CryptoCoin.CoinMarketCap.Types.Internal hiding (id)
import CryptoCoin.CoinMarketCap.Types.Quote

import Data.XHTML (Name)

data MetaData = MetaData Status (Map Idx Listing)
   deriving (Eq, Ord, Show)

mapListings :: [Listing] -> Map Idx Listing
mapListings = Map.fromList . map (idx &&& id)

instance FromJSON MetaData where
   parseJSON = withObject "Metadata" $ \v ->
      MetaData <$> v .: "status" <*> (mapListings <$> v .: "data")

instance Date MetaData where
   date (MetaData (Status d _ _ _ _ _) _) = d

data Status = Status Day Int (Maybe String) Int Int (Maybe String)
   deriving (Eq, Ord, Show)

instance FromJSON Status where
   parseJSON = withObject "Status" $ \v ->
      Status <$> (readDate <$> v .: "timestamp") <*> v .: "error_code"
             <*> v .:? "error_message" <*> v .: "elapsed"
             <*> v .: "credit_count" <*> v .:? "notice"

readDate :: String -> Day
readDate = read . take 10

data CoinInfo = CoinInfo Idx Name Symbol String Int Day
   deriving (Eq, Ord, Show)

class CoinData a where
   info :: a -> CoinInfo

instance Rank CoinInfo where
   rank (CoinInfo _ _ _ _ r _) = r

instance Named CoinInfo where
   namei (CoinInfo _ n _ _ _ _) = n

instance Cymbal CoinInfo where
   sym (CoinInfo _ _ s _ _ _) = s

instance Indexed CoinInfo where
   idx (CoinInfo i _ _ _ _ _) = i

data Coin = Coin CoinInfo
   deriving (Eq, Ord, Show)

data Token = Token { coininf :: CoinInfo, coinRef :: Idx, token :: String }
   deriving (Eq, Ord, Show)

data ECoin = C Coin | T Token
   deriving (Eq, Ord, Show)

isToken :: ECoin -> Bool
isToken (T _) = True
isToken _     = False

instance CoinData ECoin where
   info (C (Coin cd)) = cd
   info (T (Token cd _ _)) = cd

instance Rank ECoin where
   rank = rank . info

instance Named ECoin where
   namei = namei . info

instance Indexed ECoin where
   idx = idx . info

instance Cymbal ECoin where
   sym = sym . info

raw2coin :: Listing' -> ECoin
raw2coin l = r2c l (plat l)

r2c :: Listing' -> Maybe CoinRef' -> ECoin
r2c l Nothing = C (Coin (mkci l))
r2c l (Just (CR' i tok)) = T (Token (mkci l) i tok)

mkci :: Listing' -> CoinInfo
mkci (Listing' id name sym slug _num dt _cs _ts _ms _tgs _plat rank _qt) =
   CoinInfo (fromIntegral id) name sym slug (fromIntegral rank) (readDate dt)

-- FOR LISTINGS AND QUOTES --------------------------------------------

data Supplies = 
   Supplies { circulatingSupply :: Double,
              totalSupply       :: Double,
              maxSupply         :: Maybe Double }
      deriving (Eq, Ord, Show)

type Tag = String
      
data Listing =
   Listing { coin        :: ECoin,
             marketPairs :: Integer,
             supplies    :: Supplies,
             tags        :: [Tag],
             quote       :: Maybe Quote }
      deriving (Eq, Ord, Show)

l2l :: Listing' -> Listing
l2l l@(Listing' _id _name _sym _slug num dt cs ts ms tgs _plat _rank qt) =
   Listing (raw2coin l) num (Supplies cs ts ms) tgs (Map.lookup "USD" qt)

instance FromJSON Listing where
   parseJSON v = l2l <$> parseJSON v

instance Indexed Listing where
   idx = idx . coin

instance Rank Listing where
   rank = rank . coin
