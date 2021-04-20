{-# LANGUAGE OverloadedStrings #-}

module CryptoCoin.CoinMarketCap.Reports.Recommendation where

{--
We extract the recommendations from the data-store and report them out
(in a nice format, obvs).
--}

import qualified Data.ByteString.Char8 as B
import Data.Time (Day)

import Database.PostgreSQL.Simple.Types (Query(Query))

extractRecsQuery :: Day -> Query
extractRecsQuery date = Query . B.pack $ unwords [
   "select coin.cmc_id, coin.symbol, coin.name, cal.call, ind.indicator,",
   "basis.basis, rec.confidence, tt.tracked_type",
   "from recommendation rec",
   "inner join coin coin on coin.cmc_id=rec.cmc_id",
   "inner join indicator_lk ind on ind.indicator_id=rec.indicator_id",
   "inner join call_lk cal on cal.call_id=rec.call_id",
   "inner join basis_lk basis on ind.basis_id=basis.basis_id",
   "inner join j_tracked_coin_tracked_type jtctt on jtctt.tracked_coin_id=coin.cmc_id",
   "inner join tracked_type_lk tt on tt.tracked_type_id=jtctt.tracked_type_id",
   "WHERE rec.for_date=" ++ show date]
