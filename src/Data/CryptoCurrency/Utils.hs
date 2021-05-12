{-# LANGUAGE ViewPatterns #-}

module Data.CryptoCurrency.Utils where

import Data.List (isSuffixOf)
import System.Directory (listDirectory)

-- eh, just some j-random stuff I need

-- when we want to do m2 but return the value of m1 in:
--      m1 >>= pass m2

pass :: (b -> IO a) -> b -> IO b
pass proc val = proc val >> return val

pass' :: IO a -> b -> IO b
pass' proc = pass (const proc)

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

{-- Singularity and Pluralisms ... yeah, I just said that. --------------------

Singulars and plurals, are so different, bless my soul. 
Has it ever occured to you, that the plural of half,
   is whole?

 ~ Alan Sherman
--}

punct :: Int -> String
punct sz | sz == 0 = "."
         | otherwise = ":"

plural :: Int -> String
plural 1 = ""
plural _ = "s"

toBe :: Int -> String
toBe count | count == 1 = " is "
           | otherwise  = " are "
