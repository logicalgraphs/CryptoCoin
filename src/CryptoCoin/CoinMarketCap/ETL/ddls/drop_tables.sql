-- JOIN-TABLES -- (drop first) ---------------------------------------------

DROP TABLE IF EXISTS "j_tag_coin";
DROP TABLE IF EXISTS "j_tracked_coin_tracked_type";
DROP TABLE IF EXISTS "j_transaction_recommendation";

-- TABLES ------------------------------------------------------------------

DROP TABLE IF EXISTS "coin";

DROP TABLE IF EXISTS "token";

DROP TABLE IF EXISTS "tag";

DROP TABLE IF EXISTS "flipside_crypto_daily";

DROP TABLE IF EXISTS "score";

DROP TABLE IF EXISTS "source";

DROP TABLE IF EXISTS "coin_market_cap_daily_listing";

DROP TABLE IF EXISTS "portfolio";

DROP TABLE IF EXISTS "transaction_log";

DROP TABLE IF EXISTS "candlesticks";

DROP TABLE IF EXISTS "trend";

DROP TABLE IF EXISTS "recommendation";

DROP TABLE IF EXISTS "new_coin";

DROP TABLE IF EXISTS "staking";

DROP TABLE IF EXISTS "transfer_funds";

-- LOOKUP TABLES -- (drop with care) ---------------------------------------

DROP TABLE IF EXISTS "currency_lk";
DROP TABLE IF EXISTS "source_type_lk";
DROP TABLE IF EXISTS "tracked_type_lk";
DROP TABLE IF EXISTS "call_lk";
DROP TABLE IF EXISTS "basis_lk";
DROP TABLE IF EXISTS "indicator_lk";
DROP TABLE IF EXISTS "transfer_direction_lk";
