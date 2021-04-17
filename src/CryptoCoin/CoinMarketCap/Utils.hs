module CryptoCoin.CoinMarketCap.Utils where

import Data.List (isSuffixOf)
import System.Directory (listDirectory)

-- eh, just some j-random stuff I need

-- when we want to do m2 but return the value of m1 in:
--      m1 >>= pass m2

pass :: IO a -> b -> IO b
pass proc val = proc >> return val

type Suffixes = [String]

filesAtDir :: Suffixes -> FilePath -> IO [FilePath]
filesAtDir suffies dir = filter (suffixes suffies) <$> listDirectory dir
   where suffixes suffs = or . ([isSuffixOf] <*> suffs <*>) . return

{--
>>> getEnv "COIN_MARKET_CAP_DIR" >>= filesAtDir [".csv"] . (++ "/ETL")
["coins_traded_on_binance.csv","coins_traded_on_coinbase.csv"]
--}
