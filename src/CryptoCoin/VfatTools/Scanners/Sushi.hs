module CryptoCoin.VfatTools.Scanners.Sushi where

import Data.List (isInfixOf)

import CryptoCoin.VfatTools.Scanners.Types (FileScanner)
import CryptoCoin.VfatTools.Scanners.Utils
          (readFarmsWith, fastForwardP, convertOneBlock)
import CryptoCoin.VfatTools.Types (YieldFarm)

{--
The format for Sushi yield farms is as follows:

[1USDC]-[WONE] SLP [+] [-] [<=>] Price: $1,183,453.70 TVL: $10,474,154.62
1USDC Price: $1.00
WONE Price: $0.28
Staked: 8.2224 SLP ($9,730,814.46)
SUSHI Per Week: 4697.00 ($41,286.63)
WONE Per Week: 3024.00 ($848.55)
APR SUSHI: Day 0.06% Week 0.42% Year 22.06%
APR WONE: Day 0.00% Week 0.01% Year 0.45%
Total Per Week: $42,135.18
Total APR: Day 0.0619% Week 0.43% Year 22.52%
You are staking 0.00 [1USDC]-[WONE] SLP ($0.00), 0.00% of the pool.
Stake 0.00 [1USDC]-[WONE] SLP
Unstake 0.00 [1USDC]-[WONE] SLP
Claim 0.00 SUSHI ($0.00) + 0.00 WONE ($0.00)
Staking or unstaking also claims rewards.
--}

readSushiFarms :: FilePath -> IO [YieldFarm]
readSushiFarms =
   readFarmsWith (convertOneBlock 0 . fastForwardP ("TVL:" `isInfixOf`))

{--
>>> sush <- readSushiFarm "data-files/yield-farming/sushi/2021-11-20/scrape.txt"
>>> length sush
16

>>> head sush
YieldFarm {name = "[1USDC]-[WONE]", 
           coins = {("1USDC",$1.00),("WONE",$0.28)},
           tvl = $10474154.62,
           jewels = 4697.0}
--}
