module CryptoCoin.CoinMarketCap.ETL.Candlesticks.Util where

{-- 
Common candlestick load/transform resources
--}

import qualified Data.Map as Map

import Data.Time (Day)

import Control.Scan.CSV (readMaybe)

import Data.CryptoCurrency.Types (Idx, IxRow(IxRow))
import Data.CryptoCurrency.Types.OCHLV (OCHLV, OCHLVData(OCHLVData))

import Data.LookupTable (LookupTable)

fromCSV  :: Idx -> [String] -> Maybe OCHLV
fromCSV i = fc' i . readMaybe . head <*> map readMaybe . tail

fc' :: Idx -> Maybe Day -> [Maybe Double] -> Maybe OCHLV
fc' i d [o,c,h,l,a,v] =
   IxRow i <$> d <*> (OCHLVData <$> o <*> c <*> h <*> l <*> a <*> v)

cndlstks :: LookupTable -> Integer
cndlstks srcLk = srcLk Map.! "CANDLESTICKS"
