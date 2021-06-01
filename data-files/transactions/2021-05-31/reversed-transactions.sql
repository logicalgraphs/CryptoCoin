-- these transactions were reversed due to insufficient funds. Ouch.

-- also, these reversed transactions were verified by hand. Again: ouch.

-- for Gemini

delete from transaction_log where transaction_id=53;
update portfolio set cash=-800 where portfolio_id = 3;

-- for CoinBase

delete from transaction_log where transaction_id in (17, 34, 38, 35, 37);
