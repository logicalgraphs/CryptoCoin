module Data.CryptoCurrency.Types.Recommendation where

-- Houses recommendations

import Data.Char (isUpper)
import Data.List (break, intercalate)
import qualified Data.Map as Map
import Data.Time (Day)

import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

import Control.DList

import Data.CryptoCurrency.Types
import Data.LookupTable (LookupTable)
import Data.Percentage

data Call = BUY | SELL
   deriving (Eq, Ord, Show)

data RecommendationData = Rekt Idx Idx Percentage
   deriving (Eq, Ord, Show)

data Indicator = SimpleMovingAverage
               | ExponentialMovingAverage
               | MovingAverageConvergenceDivergence
               | RelativeStrengthIndex
               | OnBalanceVolume
   deriving (Eq, Ord, Show)

data Pattern = ThreeWhiteKnights
             | ThreeLineStrikes
             | ThreeBlackCrows
             | AbandonedBaby
             | TwoBlackGapping
             | EveningStar
   deriving (Eq, Ord, Show)

type Recommendation = IxRow RecommendationData

instance ToRow RecommendationData where
   toRow (Rekt call ind (P p)) =
      let d = (fromRational p) :: Double in
      [toField call, toField ind, toField d]

toRekt :: Show ind => Idx -> Day -> LookupTable -> LookupTable -> Call -> ind
                   -> Percentage -> Maybe Recommendation
toRekt coinId day callLk indLk call ind p =
   IxRow coinId day <$>
         (Rekt <$> Map.lookup (show call) callLk
               <*> Map.lookup (deCamelCase ind) indLk
               <*> Just p)

deCamelCase :: Show a => a -> String
deCamelCase = intercalate " " . flip dcc' emptyDL . show

dcc' :: String -> DList String -> [String]
dcc' "" ans = dlToList ans
dcc' (h:t) acc =
   let (w1,w2) = break isUpper t
   in  dcc' w2 (acc <| (h:w1))

{--
>>> deCamelCase SimpleMovingAverage 
"Simple Moving Average"
--}
