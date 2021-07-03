module CryptoCoin.CoinMarketCap.Reports.DayAfterResults where

{--
select b.cmc_id, c.symbol, b.for_date, b.rank, b.quote_price,
                           a.for_date, a.rank, a.quote_price
from coin_market_cap_daily_listing a
inner join coin_market_cap_daily_listing b on a.cmc_id=b.cmc_id
inner join coin c on c.cmc_id=b.cmc_id
where a.for_date ='2021-07-02' and b.for_date='2021-07-01'
and a.cmc_id in (1831,...)
order by b.rank
--}

foo :: ()
foo = ()
