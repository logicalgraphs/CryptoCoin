-- TABLES --------------------------------------------------------------------------

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

create table "source_type_lk" (
	"source_type_id" serial NOT NULL,
	"source_type" TEXT NOT NULL,
	CONSTRAINT "source_type_pk" PRIMARY KEY ("source_type_id")
) WITH (
  OIDS=FALSE
);

INSERT INTO source_type_lk (source_type_id, source_type)
VALUES (1, 'RANKING'), (2, 'LISTING'), (3, 'FCAS'), (4, 'CANDLESTICKS');

CREATE TABLE "tracked_coin" (
	"tracked_coin_id" serial NOT NULL,
	"cmc_id" bigint NOT NULL,
	"tracked_from" DATE NOT NULL,
	"tracked_price" double precision NOT NULL,
	"tracked_type_id" bigint NOT NULL,
	CONSTRAINT "tracked_coin_pk" PRIMARY KEY ("tracked_coin_id")
) WITH (
  OIDS=FALSE
);

CREATE INDEX ON tracked_coin (cmc_id);

CREATE TABLE "tracked_type_lk" (
	"tracked_type_id" serial NOT NULL,
	"tracked_type" TEXT NOT NULL,
	CONSTRAINT "tracked_type_lk_pk" PRIMARY KEY ("tracked_type_id")
) WITH (
  OIDS=FALSE
);

INSERT INTO tracked_type_lk (tracked_type_id, tracked_type)
VALUES (1, 'NEW'), (2, 'COINBASE'), (3, 'BINANCE'), (4, 'RANK_VARIANCE'),
       (5, 'PRICE_VARIANCE'), (6, 'MENTIONED_IN_NEWS');

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
	"cash" double precision NOT NULL,
	CONSTRAINT "Portfolio_pk" PRIMARY KEY ("portfolio_id")
) WITH (
  OIDS=FALSE
);



CREATE TABLE "transaction_log" (
	"transaction_id" serial NOT NULL,
	"cmc_id" bigint NOT NULL,
	"purchase_usd" double precision NOT NULL,
	"surcharge_usd" double precision NOT NULL,
	"n_coins" double precision NOT NULL,
	"transaction_type_id" bigint NOT NULL,
	CONSTRAINT "transaction_log_pk" PRIMARY KEY ("transaction_id")
) WITH (
  OIDS=FALSE
);

CREATE INDEX ON transaction_log (cmc_id);


CREATE TABLE "j_transaction_portfolio" (
	"jtp_id" serial NOT NULL,
	"portfolio_id" bigint NOT NULL,
	"transaction_id" bigint NOT NULL,
	CONSTRAINT "j_transaction_portfolio_pk" PRIMARY KEY ("jtp_id")
) WITH (
  OIDS=FALSE
);



CREATE TABLE "transaction_type_lk" (
	"transaction_type_id" serial NOT NULL,
	"transaction_type" TEXT NOT NULL,
	CONSTRAINT "transaction_type_lk_pk" PRIMARY KEY ("transaction_type_id")
) WITH (
  OIDS=FALSE
);

INSERT INTO transaction_type_lk (transaction_type_id, transaction_type)
VALUES (1, 'STRONG_BUY'), (2, 'BUY'), (3, 'HOLD'), (4, 'SELL'), (5, 'STRONG_SELL');


CREATE TABLE "candlesticks" (
	"candlestick_id" serial NOT NULL,
	"date" DATE NOT NULL,
	"open" double precision NOT NULL,
	"high" double precision NOT NULL,
	"low" double precision NOT NULL,
	"close" double precision NOT NULL,
	"adjusted_close" double precision NOT NULL,
	"volume" double precision NOT NULL,
	"currency_id" bigint NOT NULL,
	"cmc_id" bigint NOT NULL,
	CONSTRAINT "candlesticks_pk" PRIMARY KEY ("candlestick_id")
) WITH (
  OIDS=FALSE
);

CREATE INDEX ON candlesticks (cmc_id);
CREATE INDEX ON candlesticks (date);


CREATE TABLE "currency_lk" (
	"currency_id" serial NOT NULL,
	"currency" TEXT NOT NULL,
	CONSTRAINT "currency_lk_pk" PRIMARY KEY ("currency_id")
) WITH (
  OIDS=FALSE
);

INSERT INTO currency_lk (currency_id, currency) VALUES (1, 'USD');
