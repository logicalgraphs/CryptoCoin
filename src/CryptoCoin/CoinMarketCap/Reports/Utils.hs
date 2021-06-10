module CryptoCoin.CoinMarketCap.Reports.Utils where

-- we just have utils everywhere, don't we. BUT (as any Prologer will tell you)
-- IT'S FOR A GOOD CLAUSE! :<

connective :: Int -> String
connective 0 = ""
connective _ = " and"
