{-# LANGUAGE ViewPatterns #-}

module Data.CryptoCurrency.Types.Recommendation where

-- Houses recommendations

import qualified Data.ByteString.Char8 as B
import Data.Char (isLower)
import Data.List (groupBy)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Time (Day)

import Database.PostgreSQL.Simple (Connection, executeMany)
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types (Query(Query))

import Data.CryptoCurrency.Types
import Data.LookupTable (LookupTable)
import Data.Percentage

data Call = BUY | SELL
   deriving (Eq, Ord, Show)

data Source = Pat Pattern | Ind Indicator
   deriving (Eq, Ord, Show)

data RecommendationData = Rekt Call Source (Maybe Percentage)
   deriving (Eq, Ord, Show)

data Indicator = SimpleMovingAverage
               | ExponentialMovingAverage
               | MovingAverageConvergenceDivergence
               | RelativeStrengthIndex
               | OnBalanceVolume
   deriving (Eq, Ord, Show)

data Pattern = ThreeWhiteKnights
             | ThreeLineStrike
             | ThreeBlackCrows
             | AbandonedBaby
             | TwoBlackGapping
             | EveningStar
   deriving (Eq, Ord, Show)

type Recommendation = IxRow RecommendationData

data RektRow = RR' (Maybe Double) Idx Idx
   deriving (Eq, Ord, Show)

toIxRektRow :: LookupTable -> LookupTable -> Recommendation
            -> Maybe (IxRow RektRow)
toIxRektRow cLk iLk (IxRow ix date rekt) =
   IxRow ix date <$> toRektRow rekt cLk iLk

toRektRow :: RecommendationData -> LookupTable -> LookupTable -> Maybe RektRow
toRektRow (Rekt c i p) = trr' c i (fromRational . percent <$> p)

trr' :: Call -> Source -> Maybe Double -> LookupTable -> LookupTable
     -> Maybe RektRow
trr' (show -> c) (dCamelCase' -> str) p cLk iLk =
   RR' p <$> Map.lookup c cLk <*> Map.lookup str iLk

{--
>>> withConnection ECOIN (\conn -> lookupTable conn "call_lk" >>= \cLk -> 
          lookupTableFrom conn "select indicator_id,indicator FROM indicator_lk"
                                 >>= \iLk -> 
          print $ toRektRow (RekT BUY SimpleMovingAverage (P 44)) cLk iLk)
Just (RR' 0.44 1 7)

>>> withConnection ECOIN (\conn -> lookupTable conn "call_lk" >>= \cLk -> 
          lookupTableFrom conn "select indicator_id,indicator FROM indicator_lk"
                                 >>= \iLk -> 
          print $ toRektRow (RekP SELL AbandonedBaby (P 32)) cLk iLk)
Just (RR' 0.32 2 4)

... converting the other way will be ... 'fun.'
--}

instance ToRow RektRow where
   toRow (RR' d call ind) =
      [toField call, toField ind, toField d]

-- inserting a recommendation

insertRektQuery :: Query
insertRektQuery = Query . B.pack $ unwords [
   "INSERT INTO recommendation (cmc_id, for_date, call_id, indicator_id,",
   "confidence) VALUES (?, ?, ?, ?, ?)"]

insertRecommendations :: Connection -> LookupTable -> LookupTable
                      -> [Recommendation] -> IO ()
insertRecommendations conn callLk indLk recs =
   putStrLn ("Inserting " ++ show (length recs) ++ " recommendations.")        >>
   executeMany conn insertRektQuery (mapMaybe (toIxRektRow callLk indLk) recs) >>
   putStrLn "...done."

dCamelCase' :: Source -> String
dCamelCase' (Ind i) = deCamelCase i
dCamelCase' (Pat p) = deCamelCase p

deCamelCase :: Show a => a -> String
deCamelCase = unwords . groupBy (curry $ isLower . snd) . show
   -- via p h z @phaazon_

{--
>>> deCamelCase SimpleMovingAverage 
"Simple Moving Average"
--}
