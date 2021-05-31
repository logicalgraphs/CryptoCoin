-- TABLES ---------------------------------------------------------------------

CREATE TABLE "coin_market_cap_daily_listing" (
	"cmc_daily_listing_id" serial NOT NULL,
	"cmc_id" bigint NOT NULL,
	"num_pairs" integer NULL,
	"max_supply" double precision NULL,
	"circulating_supply" double precision NULL,
	"total_supply" double precision NULL,
	"quote_price" double precision NULL,
	"volume_24h" double precision NULL,
	"percent_change_1h" double precision NULL,
	"percent_change_24h" double precision NULL,
	"percent_change_7d" double precision NULL,
	"percent_change_30d" double precision NULL,
	"percent_change_60d" double precision NULL,
	"percent_change_90d" double precision NULL,
	"market_cap" double precision NULL,
	"for_date" DATE not null default now(),
	"rank" integer NOT NULL,
	"list_src_id" bigint NOT NULL,
	CONSTRAINT "coin_market_cap_daily_listing_pk" PRIMARY KEY ("cmc_daily_listing_id")
) WITH (
  OIDS=FALSE
);

CREATE INDEX ON coin_market_cap_daily_listing (cmc_id);

CREATE TABLE "coin" (
	"cmc_id" serial NOT NULL,
	"name" TEXT NOT NULL,
	"symbol" TEXT NOT NULL,
	"flipsidecrypto_id" uuid,
	"slug" TEXT,
	"date_added" date,
	CONSTRAINT "coin_pk" PRIMARY KEY ("cmc_id")
) WITH (
  OIDS=FALSE
);

CREATE TABLE "token" (
	"token_id" serial NOT NULL,
	"parent_id" bigint NOT NULL,
	"token_address" TEXT,
	CONSTRAINT "token_pk" PRIMARY KEY ("token_id")
) WITH (
  OIDS=FALSE
);

CREATE TABLE "tag" (
	"tag_id" serial NOT NULL,
	"tag_name" TEXT NOT NULL,
	CONSTRAINT "tag_pk" PRIMARY KEY ("tag_id")
) WITH (
  OIDS=FALSE
);

CREATE TABLE "j_tag_coin" (
	"j_tag_cmc_id" serial NOT NULL,
	"tag_id" bigint NOT NULL,
	"cmc_id" bigint NOT NULL,
	CONSTRAINT "j_tag_coin_pk" PRIMARY KEY ("j_tag_cmc_id")
) WITH (
  OIDS=FALSE
);

CREATE INDEX ON j_tag_coin (tag_id);
CREATE INDEX ON j_tag_coin (cmc_id);

CREATE TABLE "flipside_crypto_daily" (
	"fsc_day_id" serial NOT NULL,
	"cmc_id" bigint NOT NULL,
	"fcas_src_id" bigint NOT NULL,
	"date" DATE NOT NULL DEFAULT now(),
	"volume_usd" double precision NOT NULL,
	"transactions" integer NOT NULL,
	"unique_addresses" integer NOT NULL,
	"fcas" integer NOT NULL,
	"developer_behavior" integer NOT NULL,
	"user_activity" integer NOT NULL,
	"market_maturity" integer NOT NULL,
	"fcas_comparison" integer NOT NULL,
	CONSTRAINT "flipside_crypto_daily_pk" PRIMARY KEY ("fsc_day_id")
) WITH (
  OIDS=FALSE
);

CREATE TABLE "score" (
	"score_id" serial NOT NULL,
	"score" integer NOT NULL,
	"grade" char NOT NULL,
	CONSTRAINT "score_pk" PRIMARY KEY ("score_id")
) WITH (
  OIDS=FALSE
);

create table "source" (
	"source_id" serial NOT NULL,
	"source_type_id" bigint NOT NULL,
	"file_name" TEXT NOT NULL,
	"for_day" DATE NOT NULL DEFAULT now(),
	"processed" bool NOT NULL DEFAULT "false",
	"file" text NOT NULL,
	CONSTRAINT "source_pk" PRIMARY KEY ("source_id")
) WITH (
  OIDS=FALSE
);

create table "new_coin" (
	"new_coin_id" serial NOT NULL,
	"cmc_id" bigint NOT NULL,
	"for_date" DATE NOT NULL DEFAULT now(),
	CONSTRAINT "new_coin_pk" PRIMARY KEY ("new_coin_id")
) WITH (
  OIDS=FALSE
);

CREATE INDEX ON new_coin (for_date);

create table "source_type_lk" (
	"source_type_id" serial NOT NULL,
	"source_type" TEXT NOT NULL,
	CONSTRAINT "source_type_pk" PRIMARY KEY ("source_type_id")
) WITH (
  OIDS=FALSE
);

INSERT INTO source_type_lk (source_type_id, source_type)
VALUES (1, 'RANKING'), (2, 'LISTING'), (3, 'FCAS'), (4, 'CANDLESTICKS');

CREATE TABLE "tracked_type_lk" (
	"tracked_type_id" serial NOT NULL,
	"tracked_type" TEXT NOT NULL,
	"url" TEXT NULL,
	CONSTRAINT "tracked_type_lk_pk" PRIMARY KEY ("tracked_type_id")
) WITH (
  OIDS=FALSE
);

INSERT INTO tracked_type_lk (tracked_type_id, tracked_type, url)
VALUES (1, 'NEW', null),
       (2, 'COINBASE', 'https://www.coinbase.com'), 
       (3, 'BINANCE', 'https://www.binance.us/en/home'),
       (4, 'PHEMEX', 'https://phemex.com/'),
       (5, 'GEMINI', 'https://exchange.gemini.com/'),
       (6, 'RANK_VARIANCE', null),
       (7, 'PRICE_VARIANCE', null), 
       (8, 'MENTIONED_IN_NEWS', null),
       (9, 'BANK_ACCOUNT', null);

CREATE TABLE "j_tracked_coin_tracked_type" (
	"jtctt_id" serial NOT NULL,
	"tracked_coin_id" bigint NOT NULL,
	"tracked_type_id" bigint NOT NULL,
	CONSTRAINT "j_tracked_coin_tracked_type_pk" PRIMARY KEY ("jtctt_id")
) WITH (
  OIDS=FALSE
);

CREATE TABLE "portfolio" (
	"portfolio_id" serial NOT NULL,
	"portfolio_name" TEXT NOT NULL,
	"cash" double precision NOT NULL DEFAULT 0,
	"tracked_type_id" bigint NULL,
	CONSTRAINT "portfolio_pk" PRIMARY KEY ("portfolio_id")
) WITH (
  OIDS=FALSE
);

-- let's start us out with two portfolii ... now: three portfolii.

INSERT INTO portfolio (portfolio_id, portfolio_name, tracked_type_id)
VALUES (1, 'COINBASE', 2), (2, 'BINANCE', 3), (3, 'GEMINI', 5), (4, 'USAA', 9);

CREATE TABLE "transaction_log" (
	"transaction_id" serial NOT NULL,
	"cmc_id" bigint NOT NULL,
	"for_date" DATE NOT NULL DEFAULT now(),
	"purchase_usd" double precision NOT NULL,
	"surcharge_usd" double precision NOT NULL,
	"n_coins" double precision NOT NULL,
	"call_id" bigint NOT NULL,
	"portfolio_id" bigint NULL,
	CONSTRAINT "transaction_log_pk" PRIMARY KEY ("transaction_id")
) WITH (
  OIDS=FALSE
);

CREATE INDEX ON transaction_log (cmc_id);
CREATE INDEX ON transaction_log (for_date);
CREATE INDEX ON transaction_log (portfolio_id);

CREATE TABLE "candlesticks" (
	"candlestick_id" serial NOT NULL,
	"for_date" DATE NOT NULL DEFAULT now(),
	"open" double precision NOT NULL,
	"high" double precision NOT NULL,
	"low" double precision NOT NULL,
	"close" double precision NOT NULL,
	"adjusted_close" double precision NOT NULL,
	"volume" double precision NOT NULL,
	"currency_id" bigint NOT NULL,
	"cmc_id" bigint NOT NULL,
	"source_id" bigint NOT NULL,
	CONSTRAINT "candlesticks_pk" PRIMARY KEY ("candlestick_id")
) WITH (
  OIDS=FALSE
);

CREATE INDEX ON candlesticks (cmc_id);
CREATE INDEX ON candlesticks (for_date);


CREATE TABLE "currency_lk" (
	"currency_id" serial NOT NULL,
	"currency" TEXT NOT NULL,
	CONSTRAINT "currency_lk_pk" PRIMARY KEY ("currency_id")
) WITH (
  OIDS=FALSE
);

INSERT INTO currency_lk (currency_id, currency) VALUES (1, 'USD');


CREATE TABLE "trend" (
	"trend_id" serial NOT NULL,
	"cmc_id" bigint NOT NULL,
	"for_date" DATE NOT NULL DEFAULT now(),
	"sma_50" double precision,
	"sma_200" double precision,
	"ema_9_signal_line" double precision,
	"ema_12" double precision,
	"ema_26" double precision,
	"macd" double precision,
	"rsi_14" double precision,
	"obv" double precision,
	CONSTRAINT "trend_pk" PRIMARY KEY ("trend_id")
) WITH (
  OIDS=FALSE
);

CREATE INDEX ON trend (cmc_id);
CREATE INDEX ON trend (for_date);

CREATE TABLE "recommendation" (
	"recommendation_id" serial NOT NULL,
	"cmc_id" bigint NOT NULL,
	"for_date" DATE NOT NULL DEFAULT now(),
	"call_id" bigint NOT NULL,
	"indicator_id" bigint NOT NULL,
	"confidence" double precision,
	CONSTRAINT "recommendation_pk" PRIMARY KEY ("recommendation_id")
) WITH (
  OIDS=FALSE
);

create index on recommendation (cmc_id);
create index on recommendation (for_date);

CREATE TABLE "call_lk" (
	"call_id" serial NOT NULL,
	"call" TEXT NOT NULL,
	CONSTRAINT "call_lk_pk" PRIMARY KEY ("call_id")
) WITH (
  OIDS=FALSE
);

INSERT INTO call_lk (call_id, call)
VALUES (1, 'BUY'), (2, 'SELL');

CREATE TABLE "indicator_lk" (
	"indicator_id" serial NOT NULL,
	"indicator" TEXT NOT NULL,
	"tla" TEXT NOT NULL,
	"url" TEXT NOT NULL,
	"basis_id" bigint NOT NULL,
	CONSTRAINT "indicator_lk_pk" PRIMARY KEY ("indicator_id")
) WITH (
  OIDS=FALSE
);

insert into indicator_lk (indicator_id, indicator, tla, url, basis_id)
VALUES (1, 'Three White Knights', 'TWK',
        'https://www.investopedia.com/terms/t/three_white_soldiers.asp', 1),
       (2, 'Three Black Crows', 'TBC',
        'https://www.investopedia.com/terms/t/three_black_crows.asp', 1),
       (3, 'Three Line Strikes', 'TLS',
        'https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#three-line-strike', 1),
       (4, 'Abandoned Baby', 'AB',
        'https://www.investopedia.com/terms/b/bullish-abandoned-baby.asp', 1),
       (5, 'Two Black Gapping', 'TBG',
        'https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#two-black-gapping', 1),
       (6, 'Evening Star', 'ES',
        'https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#evening-star', 1),
       (7, 'Simple Moving Average', 'SMA',
        'https://www.investopedia.com/terms/s/sma.asp', 2), 
       (8, 'Exponential Moving Average', 'EMA',
        'https://www.investopedia.com/terms/e/ema.asp', 2),
       (9, 'Moving Average Convergence Divergence', 'MACD',
        'https://www.investopedia.com/terms/m/macd.asp', 2),
       (10, 'On Balance Volume', 'OBV',
        'https://www.investopedia.com/terms/o/onbalancevolume.asp', 3), 
       (11, 'Relative Strength Index', 'RSI',
        'https://www.investopedia.com/terms/r/rsi.asp', 2);

CREATE TABLE "basis_lk" (
	"basis_id" serial NOT NULL,
	"basis" TEXT NOT NULL,
	CONSTRAINT "basis_lk_pk" PRIMARY KEY ("basis_id")
) WITH (
  OIDS=FALSE
);

insert into basis_lk (basis_id, basis)
VALUES (1, 'CANDLESTICK'), (2, 'PRICE'), (3, 'VOLUME');


CREATE TABLE "j_transaction_recommendation" (
	"jtr_id" serial NOT NULL,
	"transaction_id" bigint NOT NULL,
	"recommendation_id" bigint NOT NULL,
	CONSTRAINT "j_transaction_recommendation_pk" PRIMARY KEY ("jtr_id")
) WITH (
  OIDS=FALSE
);

Create index on j_transaction_recommendation (transaction_id);
