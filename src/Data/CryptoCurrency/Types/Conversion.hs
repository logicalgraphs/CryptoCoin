module Data.CryptoCurrency.Types.Conversion where

-- Describes a conversion from one coin to another

import Control.Monad (void)

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection, executeMany)

import Data.CryptoCurrency.Types (IxRow(IxRow))
import Data.CryptoCurrency.Types.Coin (CoinIdsLookup)
import Data.CryptoCurrency.Types.Conversions.Internal
       (insertConversionQuery, Spot'(Spot'), Convert', ConvertData'(CD'))
import Data.CryptoCurrency.Types.Transactions.Context (TransactionContext(TaC))

import Data.LookupTable (LookupTable)

import Data.XHTML (Name)

data Spot = Spot { coin :: Name, amount, quote :: Double }
   deriving (Eq, Ord, Show)

toSpot' :: CoinIdsLookup -> Spot -> Maybe Spot'
toSpot' coinLk (Spot c a q) = Map.lookup c coinLk >>= \cix ->
   return (Spot' (head $ Set.toList cix) a q)

data ConvertData =
   CD { date                     :: Day,
        portfolio                :: Name,
        fromCoin, toCoin         :: Spot,
        coinFee, commission, tax :: Double,
        confirmation             :: Maybe Name }
      deriving (Eq, Ord, Show)

-- now we convert the above to a db-storable representation
-- (and have, of course, the dual)

toConvert' :: TransactionContext -> ConvertData -> Maybe Convert'
toConvert' (TaC coinLk _ portLk _) (CD dt port c1 c2 fee comm tx confirm) =
   Map.lookup port portLk >>= \porti ->
   toSpot' coinLk c1      >>= \s1 ->
   toSpot' coinLk c2      >>= \s2 ->
   return (IxRow porti dt (CD' s1 s2 fee comm tx confirm))

storeConvertData :: Connection -> TransactionContext -> [ConvertData] -> IO ()
storeConvertData conn tc =
   void . executeMany conn insertConversionQuery . mapMaybe (toConvert' tc)
