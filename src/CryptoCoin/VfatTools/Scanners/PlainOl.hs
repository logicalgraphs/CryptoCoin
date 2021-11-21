module CryptoCoin.VfatTools.Scanners.PlainOl where

-- Here is where we scan most the yield-farm text file-types.

import Data.List (isPrefixOf, dropWhile)

import CryptoCoin.VfatTools.Types (YieldFarm)
import CryptoCoin.VfatTools.Scanners.Utils
          (readFarmsWith, fastForwardP, convertOneBlock)

readFarms :: FilePath -> IO [YieldFarm]
readFarms = readFarmsWith (convertOneBlock 2 . fastForwardTo " - [")

{--
First iteration of scanFile where YieldFarm = String:

>>> today >>= dateDir "jewel" >>= readFarms . (++ "/scrape.txt") >>= mapM_ putStrLn . take 5
0 - [JEWEL]-[WONE] Uni LP [+] [-] [<=>] Price: $2.02 TVL: $51,097,846.16
JEWEL Price: $4.40
WONE Price: $0.22
Staked: 24970606.4902 JEWEL-LP ($50,340,218.91)
JEWEL Per Week: 7294070.35 ($32,088,439.83)
So, a YieldFarm is the one that provides the most JEWEL / USD
--}

fastForwardTo :: String -> [String] -> [String]
fastForwardTo start = fastForwardP ((start `isPrefixOf`) . dropWhile (/= ' '))

-- So: all the below works with everything but SushiSwap, because SushiSwap
-- just has to be a little bit/enough different to break my scanner. Hard.

{--
We now have:

>>> today >>= dateDir "kingdoms" >>= readFarms . (++ "/scrape.txt") >>= mapM_ print
YieldFarm {name = "[JEWEL]-[WONE]", tvl = $51097846.16, jewels = 7294070.35}
YieldFarm {name = "[JEWEL]-[BUSD]", tvl = $3789566.95, jewels = 607839.2}
YieldFarm {name = "[JEWEL]-[bscBNB]", tvl = $3238734.51, jewels = 607839.2}
YieldFarm {name = "[1ETH]-[JEWEL]", tvl = $4208499.00, jewels = 607839.2}
YieldFarm {name = "[WONE]-[BUSD]", tvl = $5476445.84, jewels = 121567.84}
YieldFarm {name = "[JEWEL]-[XYA]", tvl = $1717237.62, jewels = 303919.6}
YieldFarm {name = "[JEWEL]-[1USDC]", tvl = $3906428.91, jewels = 607839.2}
YieldFarm {name = "[1WBTC]-[JEWEL]", tvl = $4035600.15, jewels = 607839.2}
YieldFarm {name = "[UST]-[JEWEL]", tvl = $3434142.36, jewels = 607839.2}
YieldFarm {name = "[1ETH]-[WONE]", tvl = $6290152.85, jewels = 121567.84}

... along with the embedded coin prices.
--}
