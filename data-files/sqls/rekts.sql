with todays as (
select c.cmc_id cmc_id,c.symbol sym,c.name as name,quote_price price,rank
from coin_market_cap_daily_listing cmc
inner join coin c on c.cmc_id=cmc.cmc_id
where for_date=current_date
and c.cmc_id in (
1839,74,1831,3717,512,1321,3077,1765,131,6945,4066,5665,2130,
2469,6538,1697,2577,1896,1567,5617,6210,1772,3640,3012,3992,
1816,7226,4679,1637,1104,2780,5026,9421,8613
)
),
last_days as (
select r.cmc_id cmc_id,max(for_date) last_date
from recommendation r 
inner join todays t on t.cmc_id=r.cmc_id
where for_date < current_date
group by 1
),
diffs as (
select t.cmc_id,sym,name,'$' || price price,t.rank,last_date,'$' || quote_price last_price,
cmc.rank last_rank,((price - quote_price) / price * 100) diff
from todays t
inner join last_days l on l.cmc_id=t.cmc_id
inner join coin_market_cap_daily_listing cmc on cmc.cmc_id=l.cmc_id 
and l.last_date=cmc.for_date
order by rank
),
wows as (
select cmc_id, (case when diff < -10 then '???' 
when diff > 10 then '!!!'
else ' ' end) as wow
from diffs
),
xacts as (
select d.cmc_id as cmc_id,portfolio_id,for_date dt
from transaction_log t
inner join diffs d on d.cmc_id=t.cmc_id
),
xfers as (
select d.cmc_id as cmc_id, transfer_to as portfolio_id,for_date dt
from transfer_coin t
inner join diffs d on d.cmc_id=t.cmc_id
),
xs_raw as (
select * from xfers union select * from xacts
),
xs_maxs as (
select cmc_id,max(dt) max_dt
from xs_raw
group by 1
),
xs as (
select a.cmc_id cmc_id,portfolio_id,dt
from xs_raw a
inner join xs_maxs m on m.cmc_id=a.cmc_id and m.max_dt=a.dt
group by 1,2,3
)
select d.cmc_id,sym,name,'$' || price price,rank,last_date,
'$' || last_price last_price, last_rank, diff || '%' diff,wow,
(case when x.portfolio_id is null then ' ' else portfolio_name end ) portfolio
from diffs d
inner join wows w on w.cmc_id=d.cmc_id
left join xs x on x.cmc_id=d.cmc_id
left join portfolio p on p.portfolio_id=x.portfolio_id
order by rank
