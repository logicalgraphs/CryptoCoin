module Swap where

import Control.Monad (guard)

import Data.List (intercalate)

import Control.Presentation (laxmi)
import Data.Percentage (Percentage(P))

data Row = Row { pair                  :: String,
                 liquidity, volume_24h :: Double,
                 commission_apr        :: Percentage }
   deriving (Eq, Ord, Show)

thing :: IO ()
thing = readFile "terraswap.txt" >>=
        mapM_ putStrLn
          . intercalate ["UNION"] . map (pure . sqlify)
          . reverse
          . doTheThing [] . lines

doTheThing :: [Row] -> [String] -> [Row]
doTheThing rows [] = rows
doTheThing rows lines = uncurry doTheThing (snarfRow lines rows)

snarfRow :: [String] -> [Row] -> ([Row], [String])
snarfRow (a:b:c:d:rest) rows =
   ((Row a (readUST b) (readUST c) (read (despace d)):rows), rest)

sqlify :: Row -> String
sqlify (Row p l v (P c)) =
   concat ["SELECT '", p, "' AS LP, ", show l, " AS \"Liquidity (UST)\", ",
           show  v, " AS \"24hr Volume (UST)\", ", laxmi 2 c,
           " AS \"Commission APR (%)\""]
          
readUST :: String -> Double
readUST s = head (reads s >>= \(val, rest) ->
   let (mult, ust) = break (== ' ') rest in
   guard (ust == " UST") >>
   return (val * multiplier mult))

multiplier :: String -> Double
multiplier "" = 1
multiplier "K" = multiplier "" * 1000
multiplier "M" = multiplier "K" * 1000 -- and who says programming isn't fun?

despace :: String -> String
despace = filter (/= ' ')
