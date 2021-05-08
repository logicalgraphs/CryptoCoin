{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module CryptoCoin.CoinMarketCap.Reports.Coins where

{--
Okay, ecoins.sh downloads the JSON of listings for us to process. Here, we
report on the files processed.
--}

import Data.Aeson (decode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable (toList)
import Data.List (sortOn, splitAt)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Time (Day)
import Data.Time.Calendar (toGregorian)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import CryptoCoin.CoinMarketCap.Types 
            (NewCoins, NewCoinsCtx, MetaData(MetaData), coin, Listings,
             fetchTokens, fetchListingsAndTop10)
import CryptoCoin.CoinMarketCap.Reports.Table (report', newCoins,
         top10, coinHeaders, ec2cc)
import CryptoCoin.CoinMarketCap.Reports.Utils (connective, tweet)

import Data.CryptoCurrency.Types (rank, Indexed, idx)
import Data.CryptoCurrency.Utils (plural)

import Data.Time.TimeSeries (today)

import Store.SQL.Connection (withConnection, Database(ECOIN))
import Store.SQL.Util.Indexed (Index(Idx))

ranking :: Foldable t => Indexed i => Day -> Listings -> (t i, t i) -> IO ()
ranking date ecoins newsies =
   let sortCoins = sortOn rank . mapMaybe (ec2cc ecoins . coin) . toList in
   report' (show $ top10 date) coinHeaders 
           (take 10 (sortCoins $ Map.elems ecoins)) >>
   newCoins ecoins newsies

{--
>>> let date = (read "2021-02-22") :: Day
>>> ranking date coins
<p>The top-10 e-coins for 2021-02-22 (ranked by 
   <a href='https://coinmarketcap.com'>coinmarketcap.com</a> ) are:</p>
   <table border="1">
    <tr><th align="left">Rank</th><th align="left">Name</th>
        <th align="left">Symbol</th><th align="left">Type</th>
    </tr>
    <tr><td>1</td><td>Bitcoin</td><td>BTC</td><td>Coin</td></tr>
...
--}

-- Looking at the below arithmetic, I can see that George Boole was onto 
-- something!

coinsNtokens :: Foldable t => t a -> t b  -> String
coinsNtokens (length -> c) (length -> t) = c' (c + t) c t

c' :: Int -> Int -> Int -> String
c' 0 _ _ = ""
c' _ c t = " with" ++ n "coin" c ++ connective (c * t) ++ n "token" t

n :: String -> Int -> String
n _ 0 = ""
n typ c = ' ':show c ++ " new " ++ typ ++ plural c

{--
Okay.

Now we need to reimagine this reporting tool to be an independent component.

How?

We extract new_coin (ids), token (as a coin-lookup table), and the coin-infos,
maybe? ... for today, partition the new coins into coins and tokens by the
token-lookup table, then run the above functions.
--}

newCoinsQuery :: Day -> Query
newCoinsQuery date = Query . B.pack $ concat [
   "SELECT cmc_id FROM new_coin WHERE for_date='", show date, "'"]

type CoinsTokensIdxn = (Set Index, Set Index)

instance Indexed Index where
   idx (Idx i) = i

fetchNewCoinsAndTokens :: Connection -> Day -> Set Index -> IO CoinsTokensIdxn
fetchNewCoinsAndTokens conn date toks =
   Set.partition (not . flip Set.member toks) . Set.fromList
   <$> query_ conn (newCoinsQuery date)

runReport :: Connection -> Day -> IO ()
runReport conn tday =
   fetchTokens conn                               >>= \toks ->
   let tokIds = Set.map Idx (Map.keysSet toks) in
   fetchNewCoinsAndTokens conn tday tokIds        >>= \news ->
   let ucf = flip uncurry news
       idxn = map idx (Set.toList (ucf Set.union))
       msg = "The top-10 e-coins " ++ ucf coinsNtokens in
   fetchListingsAndTop10 conn tday toks idxn      >>= \lists ->
   ranking tday lists news                        >>
   tweet tday "top-10-e-coins-for-" msg           >>
   putStrLn ("Top-10 E-coins for " ++ show tday)

go :: IO ()
go = today >>= withConnection ECOIN . flip runReport
