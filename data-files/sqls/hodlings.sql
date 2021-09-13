-- computes hodlings by coin, ordered by percentage

-- now includes symbols ($,%) to ease the eyes
-- now includes claimed rewards from staked coins

with trans as (select t.cmc_id as idx,c.symbol as sym,t.n_coins as n,
t.purchase_usd as invested,
(case when t.call_id=1 then 1
when t.call_id=4 then 1
else -1 end) as mult
from transaction_log t
inner join coin c on c.cmc_id=t.cmc_id),
tots as (select idx,sym, sum(n*mult) as amt,sum(invested*mult) as inv
from trans
group by idx,sym
order by idx),
prices as (select p.for_date as date,idx,sym,amt,p.quote_price as price,
(case when inv > 1 then inv else 0 end) as inv
from tots
inner join coin_market_cap_daily_listing p on p.cmc_id=idx
where p.for_date=(select max(for_date) from coin_market_cap_daily_listing)),
values as (select date,idx,sym,amt,price,amt*price as usd_value,inv
from prices),
portfolios as
(select date,idx,sym,amt,price,usd_value,inv,
(select sum(usd_value) from values) as alles
from values
group by date,sym,idx,amt,price,usd_value,inv)
select date,idx,sym,amt,'$' || price as price, '$' || usd_value as usd_value,(usd_value / alles) || '%' as percentage,
'$' || inv as invested,'$' || (usd_value - inv) as "gain/loss",
(case when inv > 1 then ((usd_value - inv) / inv) || '%' else 'N/A' end) as "gain (%)"
from portfolios
order by (usd_value / alles) desc
