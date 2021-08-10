with this_data as 
  (
    SELECT sum(voting_power) as total_delegated
      FROM (
          SELECT Date(block_timestamp) AS date,
               address,
               p.label,
               voting_power
        FROM   terra.validator_voting_power
               LEFT OUTER JOIN terra.validator_labels p
                        ON address = vp_address
        WHERE  block_timestamp = (SELECT Max(block_timestamp)
                                  FROM   terra.validator_voting_power)
        ORDER  BY 4 DESC
)
),
  voters as (
  SELECT 
  p.label validator, tvvp.voting_power voting_power,
              (tvvp.voting_power/total_delegated)*100 voting_power_percent
        FROM   terra.validator_voting_power tvvp
               LEFT OUTER JOIN terra.validator_labels p
                        ON address = vp_address
                  FULL OUTER JOIN this_data TD
  WHERE  block_timestamp = (SELECT Max(block_timestamp)
                                  FROM   terra.validator_voting_power)
        ORDER  BY voting_power_percent DESC),
  enumerated_voters as (
  select 
  row_number() over (order by voting_power_percent desc ) row_number,
  validator, voting_power, voting_power_percent
  from voters
  ),

  select row_number, validator, voting_power, voting_power_percent,
    sum(voting_power_percent) over (order by row_number) running_percent
  from enumerated_voters
order by row_number
