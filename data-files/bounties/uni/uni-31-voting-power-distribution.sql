-- https://velocity-app.flipsidecrypto.com/dashboard/uni-31-voting-power-distribution-QyODsZ

select from_address as address, count(function_name) as votes
from ethereum.transactions
where block_timestamp > dateadd(month, -1, getdate())
  and function_name='delegate'
  and symbol='UNI'
group by address
order by votes desc
limit 10
