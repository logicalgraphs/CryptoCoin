select 
  
  TX_TO_ADDRESS_NAME curve_event, count(1) n 
  
  FROM
ethereum.events_emitted
  where tx_to_label = 'curve fi'
group by 1
order by 2 desc 
