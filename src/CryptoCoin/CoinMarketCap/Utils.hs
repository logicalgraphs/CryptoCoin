module CryptoCoin.CoinMarketCap.Utils where

-- eh, just some j-random stuff I need

pass :: IO a -> b -> IO b
pass proc val = proc >> return val
