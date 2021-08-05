select date(block_timestamp) as date, symbol, stddev_pop(price_usd) as volatility
  from terra.oracle_prices
  where symbol in ('LUNA', 'MIR', 'ANC')
and date(block_timestamp) > CURRENT_DATE - 15
and date(block_timestamp) < CURRENT_DATE
group by date, symbol
