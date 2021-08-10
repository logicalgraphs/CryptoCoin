select date(block_timestamp) date, platform, count(1) n
from ethereum.dex_swaps
where year(block_timestamp) > 2020 and platform like 'uniswap-v%'
group by date, platform 
