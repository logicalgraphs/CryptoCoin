WITH prices as(
  SELECT
    currency,
    symbol
  FROM terra.oracle_prices
  WHERE block_timestamp >= CURRENT_DATE - 7
  GROUP BY 1,2
)
SELECT 
  date(m.block_timestamp) as date,
  sum(msg_value:execute_msg:provide_liquidity:assets[0]:amount / POW(10,6)) as token_0_amount,
  t0.address_name as token0,
  sum(msg_value:execute_msg:provide_liquidity:assets[1]:amount / POW(10,6)) as token_1_amount,
  t1.symbol as token1
FROM terra.msgs m

LEFT OUTER JOIN terra.labels t0
  ON msg_value:execute_msg:provide_liquidity:assets[0]:info:token:contract_addr = t0.address
  
LEFT OUTER JOIN prices t1
  ON msg_value:execute_msg:provide_liquidity:assets[1]:info:native_token:denom::string  = t1.currency

LEFT OUTER JOIN terra.labels c
  ON msg_value:contract  = c.address
  
  
WHERE --msg_value:contract = 'terra1amv303y8kzxuegvurh0gug2xe9wkgj65enq2ux' --MIR UST POOL
  -- AND 
  msg_value:execute_msg:provide_liquidity IS NOT NULL --Ensures we only look for adding liquidity events
  -- AND m.tx_id = 'A7C9FA7E03E1363E331B9F6100828BF8A602829CC9DBCC2BD2C5DDCB60BE0AA1'
  AND date(m.block_timestamp) > CURRENT_DATE - 15
  and date(m.block_timestamp) < CURRENT_DATE
  and token0='bLUNA'
  group by date, token0, token1
ORDER BY date
LIMIT 100
