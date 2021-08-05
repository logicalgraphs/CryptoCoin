with lunae as (select date(block_timestamp) as date, stddev_pop(price_usd) as volatility
  from terra.oracle_prices
  where symbol='LUNA'
and block_timestamp > CURRENT_DATE - 15
and block_timestamp < CURRENT_DATE
group by date),
eths as ( select distinct date(hour) as date, stddev_pop(price) as volatility
from ethereum.token_prices_hourly 
where symbol='ETH' and hour > CURRENT_DATE - 15
and hour < CURRENT_DATE
  group by date)
  select 'LUNA' as coin, * from lunae
  union 
  select 'ETH' as coin, * from eths
  order by date 
