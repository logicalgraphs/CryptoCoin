select for_date, c.cmc_id, c.symbol, quote_price
from coin_market_cap_daily_listing cmcdl
inner join coin c on c.cmc_id=cmcdl.cmc_id
where for_date=(select max(for_date)
from coin_market_cap_daily_listing)
and c.cmc_id in (7278,2010,
4030,8857,3794,1839,1,1027,6538,4943,
74,1027,4172,8001,3890,8003,10767,
7857,3945,1684,2577,5426,11290,7083,3408,
7129,2011,1896)
order by c.symbol
