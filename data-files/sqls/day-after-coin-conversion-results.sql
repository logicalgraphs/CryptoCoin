-- computes the day-after results of coin conversions

with coins as (select cmc_id,symbol from coin where symbol in ('MANA','LTC','ADA', 'YFI', 'CRV', 'DAI')),
quotes as (select cmc.cmc_id as cid,symbol,for_date,quote_price
from coin_market_cap_daily_listing cmc
inner join coins m on m.cmc_id=cmc.cmc_id
where for_date in ('2021-07-31', '2021-07-30'))
select a.symbol, a.for_date, a.quote_price, b.quote_price as yest_price,
((a.quote_price - b.quote_price) / a.quote_price) as gain
from quotes a
inner join quotes b on a.cid=b.cid
where b.for_date='2021-07-30' and a.for_date='2021-07-31'

-- you have to plug in affected coins and dates yourself
