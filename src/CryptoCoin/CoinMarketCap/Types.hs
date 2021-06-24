{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.Types where

-- Types specific to CoinMarketCap data analysis

import Control.Arrow ((&&&))

import Data.Aeson

import qualified Data.ByteString.Char8 as B

import Data.Foldable (toList)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe, fromMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Time

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types

import Control.Map (snarf)

import CryptoCoin.CoinMarketCap.Types.Internal hiding (id)
import CryptoCoin.CoinMarketCap.Types.Quote

import Data.CryptoCurrency.Types

import Data.XHTML (Name)

import Store.SQL.Util.Indexed (IxValue(IxV), ix, val)

type Listings = Map Idx Listing

data MetaData = MetaData Status Listings
   deriving (Eq, Ord, Show)

mapIndexed :: Indexed i => [i] -> Map Idx i
mapIndexed = Map.fromList . map (idx &&& id)

instance FromJSON MetaData where
   parseJSON = withObject "Metadata" $ \v ->
      MetaData <$> v .: "status" <*> (mapIndexed <$> v .: "data")

instance Date MetaData where
   date (MetaData (Status d _ _ _ _ _) _) = d

data Status = Status Day Integer (Maybe String) Integer Integer (Maybe String)
   deriving (Eq, Ord, Show)

instance FromJSON Status where
   parseJSON = withObject "Status" $ \v ->
      Status <$> (readDate <$> v .: "timestamp") <*> v .: "error_code"
             <*> v .:? "error_message" <*> v .: "elapsed"
             <*> v .: "credit_count" <*> v .:? "notice"

readDate :: String -> Day
readDate = read . take 10

data CoinInfo = CoinInfo Idx Name Symbol String Integer Day
   deriving (Eq, Ord, Show)

class CoinData a where
   info :: a -> CoinInfo

instance Rank CoinInfo where
   rank (CoinInfo _ _ _ _ r _) = r

instance Cymbal CoinInfo where
   sym (CoinInfo _ _ symb _ _ _) = symb

instance Named CoinInfo where
   namei (CoinInfo _ n _ _ _ _) = n

instance Indexed CoinInfo where
   idx (CoinInfo i _ _ _ _ _) = i

data RanklessCoinInfo = RCI Name Symbol String

type CoinMap = Map Idx (IxRow RanklessCoinInfo)

instance FromRow RanklessCoinInfo where
   fromRow = RCI <$> field <*> field <*> field

fetchCoinsQuery :: Query
fetchCoinsQuery =
   "SELECT cmc_id, date_added, name, symbol, slug FROM coin"

fetchCoins' :: Connection -> IO [IxRow RanklessCoinInfo]
fetchCoins' = flip query_ fetchCoinsQuery

fetchCoins :: Connection -> IO CoinMap
fetchCoins conn = mapIndexed <$> fetchCoins' conn

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

instance Indexed ECoin where
   idx = idx . info

instance Cymbal ECoin where
   sym = sym . info

instance Named ECoin where
   namei = namei . info

raw2coin :: Listing' -> ECoin
raw2coin l = r2c l (plat l)

r2c :: Listing' -> Maybe CoinRef' -> ECoin
r2c l Nothing = C (Coin (mkci l))
r2c l (Just (CR' i tok)) = T (Token (mkci l) i tok)

type NewCoins = ([ECoin], [ECoin])
type NewCoinsCtx = (Listings, NewCoins)

-- FOR LISTINGS AND QUOTES --------------------------------------------

mkci :: Listing' -> CoinInfo
mkci (Listing' id name sym _ num dt _cs _ts _ms _tgs _plat Nothing _qt) =
   error (unwords [show id, name, sym, show num, dt, "has no rank"])
mkci (Listing' id name sym slug _num dt _cs _ts _ms _tgs _plat (Just r) _qt) =
   CoinInfo id name sym slug r (readDate dt)

data Supplies = 
   Supplies { circulatingSupply :: Double,
              totalSupply       :: Double,
              maxSupply         :: Maybe Double }
      deriving (Eq, Ord, Show)

data Tag = Tag { tag :: String }
   deriving (Eq, Ord, Show)

instance FromRow Tag where
   fromRow = Tag <$> field

instance ToField Tag where
   toField (Tag t) = toField t

fetchTagsQuery :: Query
fetchTagsQuery = Query . B.pack $ unlines [
   "SELECT jtc.cmc_id, t.tag_name FROM tag t",
   "INNER JOIN j_tag_coin jtc ON jtc.tag_id=t.tag_id",
   "WHERE jtc.cmc_id IN ?"]

type TagMap = Map Idx (Set Tag)

fetchTags :: Foldable t => Connection -> t Idx -> IO TagMap
fetchTags conn idxn =
   snarf (return . (idx &&& val))
   <$> query conn fetchTagsQuery (Only (In (toList idxn)))
      
data Listing =
   Listing { coin        :: ECoin,
             marketPairs :: Maybe Integer,
             supplies    :: Supplies,
             tags        :: Set Tag,
             quote       :: Maybe Quote }
      deriving (Eq, Ord, Show)

l2l :: Listing' -> Listing
l2l l@(Listing' _id _name _sym _slug num dt cs ts ms tgs _plat _rank qt) =
   Listing (raw2coin l) num (Supplies cs ts ms) (Set.fromList (map Tag tgs))
           (Map.lookup "USD" qt)

instance FromJSON Listing where parseJSON v = l2l <$> parseJSON v
instance Indexed Listing where idx = idx . coin
instance Rank Listing where rank = rank . coin

-- Database extraction --------------------------------------------------

data TokenRow = TokenRow Idx TokenAddress
   deriving (Eq, Ord, Show)

instance FromRow TokenRow where
   fromRow = TokenRow <$> field <*> field

instance Indexed (IxValue a) where
   idx = ix

fetchTokenRowsQuery :: Query
fetchTokenRowsQuery = "SELECT token_id,parent_id,token_address FROM token"

fetchTokens' :: Connection -> IO [IxValue TokenRow]
fetchTokens' = flip query_ fetchTokenRowsQuery

type TokenMap = Map Idx TokenRow

fetchTokens :: Connection -> IO TokenMap
fetchTokens conn = Map.map val . mapIndexed <$> fetchTokens' conn

ldb2l :: TokenMap -> CoinMap -> TagMap -> IxListingDB -> Maybe Listing
ldb2l tm cm tagm l@(IxRow idx _ (ListingDB np mbms cs ts _rnk q)) =
   tokenize (Map.lookup idx tm) (Map.lookup idx cm) l >>= \ecoin ->
   let tags = fromMaybe Set.empty (Map.lookup idx tagm) in
   return (Listing ecoin np (Supplies cs ts mbms) tags (Just q))

tokenize :: Maybe TokenRow -> Maybe (IxRow RanklessCoinInfo)
         -> IxListingDB -> Maybe ECoin
tokenize (Just (TokenRow pid ta)) c l =
   c >>= \c' -> return (T (Token (mkc c' l) pid ta))
tokenize Nothing c l = c >>= \c' -> return (C (Coin (mkc c' l)))

mkc :: IxRow RanklessCoinInfo -> IxListingDB -> CoinInfo
mkc (IxRow i _ (RCI nm sy sl)) (IxRow _ d (ListingDB _ _ _ _ rnk _)) =
   CoinInfo i nm sy sl rnk d

fetchTop10Listings :: Connection -> Day -> TokenMap -> CoinMap -> IO [Listing]
fetchTop10Listings conn tday tm cm =
   fetchTop10ListingDBs conn tday >>= \top10 ->
   fetchTags conn (map idx top10) >>=
   return . flip mapMaybe top10 . ldb2l tm cm

fetchListings' :: Foldable t => Connection -> Day -> TokenMap -> CoinMap
               -> t Idx -> IO [Listing]
fetchListings' conn tday toks coins ixn =
   fetchTags conn ixn                   >>= \tags ->
   fetchListingDBs conn tday ixn        >>=
   return . mapMaybe (ldb2l toks coins tags)

fetchListings :: Foldable t => Connection -> Day -> t Idx -> IO Listings
fetchListings conn tday idxn =
   fetchTokens conn                         >>= \toks ->
   fetchCoins conn                          >>= \coins ->
   fetchListings' conn tday toks coins idxn >>=
   fetchTokenParents conn tday toks coins . mapIndexed

fetchListingsAndTop10 :: Foldable t => Connection -> Day -> TokenMap -> t Idx
                      -> IO Listings
fetchListingsAndTop10 conn tday toks ixn =
   fetchCoins conn                         >>= \coins ->
   fetchListings' conn tday toks coins ixn >>= \news ->
   fetchTop10Listings conn tday toks coins >>=
   fetchTokenParents conn tday toks coins . mapIndexed . (news ++)

fetchTokenParents :: Connection -> Day -> TokenMap -> CoinMap -> Listings
                  -> IO Listings
fetchTokenParents conn date tm cm ls =
   let tokIds = Set.intersection (Map.keysSet tm) (Map.keysSet ls)
       tokIdsL = Set.toList tokIds
       parentId (coin -> (T (Token _ci pi _))) = pi
       pid idx = Map.lookup idx ls
       parentIds = Set.fromList (mapMaybe (\k -> parentId <$> pid k) tokIdsL)
       newIds = Set.difference parentIds tokIds
   in  if newIds == Set.empty
       then return ls
       else Map.union ls . mapIndexed <$> fetchListings' conn date tm cm newIds

-- I was gonna recurse to fetchTokenParents, but why, ya know.
