with days as (select date(hour) as date
  from ethereum.token_prices_hourly
where  hour > CURRENT_DATE - 15
and hour < CURRENT_DATE)
select distinct d.date as date, t.price as price
from days d 
inner join ethereum.token_prices_hourly t on t.hour=d.date 
where symbol='ETH'
order by date 
