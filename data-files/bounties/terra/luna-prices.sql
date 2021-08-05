select date(block_timestamp) as date, price_usd
  from terra.oracle_prices
  where symbol='LUNA'
and block_timestamp > CURRENT_DATE - 15
and block_timestamp < CURRENT_DATE
order by date
