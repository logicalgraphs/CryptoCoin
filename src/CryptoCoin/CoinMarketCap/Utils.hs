{-# LANGUAGE ViewPatterns #-}

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

-- I do this often enough now, so:

report :: Int -> String -> IO a -> IO ()
report (indent -> ind) msg a = ind msg >> a >> ind "...done"

indent :: Int -> String -> IO ()
indent i = putStrLn . (replicate i ' ' ++)

fake :: Int -> String -> IO a -> IO ()
fake (indent -> ind) msg _ = ind ("FAKING: " ++ msg)
