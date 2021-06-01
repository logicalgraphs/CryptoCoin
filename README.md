# CoinMarketCap analyzer

Here are the scripts and files for extracting the daily rankings from
coinmarketcap.com and then analyzing those coins.

## Setup

You need the following environmental variables established in order to run
this system:

* `COIN_MARKET_CAP_DIR` pointing to this directory; and,
* `COIN_MARKET_CAP_API_KEY` which you get by requesting an API key from 
coinmarketcap.com

To connect to your SQL database, you'll need the following environmental 
variables:

* `SQL_DAAS_SERVER_URL_ECOIN`
* `SQL_DAAS_DB_NAME_ECOIN`
* `SQL_DAAS_USERNAME_ECOIN`
* `SQL_DAAS_PASSWORD_ECOIN`
* `SQL_DAAS_SERVER_PORT_ECOIN`

## What to do

The script `scripts/report.sh` fetches the latest e-coin rankings, updates
our historical data, then reports on the Top-10 e-coins as well as new coins
today. It also formats a tweet and title for today's report.

The report generates the top-10 e-coins and also the new coins for the day
(state is maintained in rankMatrix). A sample report is here:
http://logicalgraphs.blogspot.com/2021/03/top-10-e-coins-for-2021-03-09.html

## E/R Diagram

<img src="src/CryptoCoin/CoinMarketCap/ETL/imgs/e-coin-erd.png"/>

## Architecture
### Recommendation Architecture

<img src="src/CryptoCoin/CoinMarketCap/design/ecoins.png"/>

The component architecture is straightforward. 

* We have loaders the pull the data from from various sources and then load 
those data to the data-store (data model, above). 
* We have processors that transform the raw (CSV and JSON), stored, data to 
entities and relations.
* We have business logic that runs patterns and detect trends in the stored
entities, then distills the results with recommendations, which are then saved
to the data-store.
* Finally, we generate reports against the entities and the recommendations
in the data store.

### Portfolio Architecture

<img src="/src/CryptoCoin/CoinMarketCap/design/portfolio.png"/>

The portfolio is a little dance between the transactions, the recommendations
associated with the transcations, culminating into a trading portfolio. There
can be multiple portfolii, and I'm thinking one per e-coin exchange. A buy/sell
transaction has an associated set of recommendations that prompted that 
transaction (indexed by the coin transacted). The price and surcharges are
transaction-based (not price-of-the-day for the coin, as the buy/sell price
fluctuates per transaction). The portfolio is the sum of the costs of the 
transactions verses the current values of the holdings.

## Patterns used

### Candlesticks

First, the [Five Most Powerful Candlestick Patterns](https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp), according to [investopedia](https://www.investopedia.com/).

* Three White Knights/Soldiers: https://www.investopedia.com/terms/t/three_white_soldiers.asp
* Three Black Crows: https://www.investopedia.com/terms/t/three_black_crows.asp
* Three Line Strike: https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#three-line-strike
* Abandoned Baby: https://www.investopedia.com/terms/b/bullish-abandoned-baby.asp
* Two Black Gapping: https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#two-black-gapping
* Evening Star: https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#evening-star

### Price and Volume trends

* Simple Moving Average: https://www.investopedia.com/terms/s/sma.asp
* Exponential Moving Average: https://www.investopedia.com/terms/e/ema.asp
* Moving Average Convergence/Diverence: https://www.investopedia.com/terms/m/macd.asp
* On Balance Volume: https://www.investopedia.com/terms/o/onbalancevolume.asp
* Relative Strength Index: https://www.investopedia.com/terms/r/rsi.asp 

## TODOs:

* Track which crypto can be loaned/earn interest. This might be useful for
Bayesian analysis, as well.

* start building predictive model: STARTED

* provide year-to-date transaction history with the portfolio report

* we need to analyze the trades vis-à-vis recommendations and find which ones
are effective and which ones aren't, or combinations, or what. ... Bayes?

* true pipeline with jobs starting on the signal of a job's completion.

* represent coins-as-graph

* create a spot-check that runs the numbers on any arbitrary yahoo-symbol

* build predictive model based off of recommendations

* download FCAS scores: ... once we have cmc_id-to-flipside_crypto_uuid mapping
  * map FCAS coin uuids to cmc_ids

* get FCAS data for monitored coins

* get d3.js tools working, e.g.s:

### d3.js has the following:

* https://observablehq.com/@d3/bollinger-bands
* https://observablehq.com/@d3/candlestick-chart
* https://observablehq.com/@fil/plateau-detection?collection=@fil/interpolation
* https://observablehq.com/@fil/hello-loess?collection=@fil/interpolation
* https://observablehq.com/@fil/gaussian-smoothing

Do we look at all e-coins as Voronoi? or Word-cloud?

* https://observablehq.com/@d3/voronoi-labels
* https://observablehq.com/@d3/word-cloud

... I have examples of running d3 under 
https://github.com/geophf/1HaskellADay/tree/master/exercises/HAD/Graph/D3

## Revision History
### ... or feeping creatures! AHA!

* 2021-06-01: Updated reports: portfolii show even if 'next day' (UTC)
* 2021-05-29: Transaction reports now as CSV
* 2021-05-28: Reports now output as CSV (except transactions, TODO)
* 2021-05-27: removed new coins report. It was nice, but unnecessary.
* 2021-05-25: Summary report across all portfolii and daily transactions report
* 2021-05-17: Storing transactions in CSV files under 
  `data-files/transactions/DATE` (both not-recommended.csv and recommended.csv)
* 2021-05-12: Transaction management and portfolio reports.
* 2021-05-07: added `verify`-target to Makefile, verifying every haskell file;
  added gemini exchange.
* 2021-05-02: Today was the first day the system ran end-to-end from ingesting
  coin-raw data to rendering recommendation reports (that weren't wrong, so,
  that's a plus).
* 2021-04-30: reports on coins now integrated into pipeline
* 2021-04-29: Separated monolith into loader, processor, patterns, reports
  pipelines.
* 2021-04-23: We can now run recommendation reports (RRR or R3 or Triple-R)
* 2021-04-18: Automated uploading which coins we are tracking from CSV files.
* 2021-04-13: Finished On Balance Volume; switched to list-monad for
trend-processing; saving today's trends to the data-store.

* 2021-04-12: Transitioned data-store from elephantsql.com to clever-cloud.com
* 2021-04-11: Relative Strength Index
* 2021-04-09: Moving Averages convergence/divergence (macd)
* 2021-04-07: using trends for trend-computations. Implemented ema (Exponential
Moving Average).

* 2021-04-04: Created a PriceVolume-type. Implemented sma (Simple Moving 
Average).

* 2021-03-31: downloading candlesticks from yahoo! and storing in database.

* 2021-03-27: uploaded coinbase and binance tradeable coins to tracked-coins.

* 2021-03-26: Saving Cryptocoin-tags to data-store:
http://logicalgraphs.blogspot.com/2021/03/saving-cryptocoin-tags-to-data-store.html

* 2021-03-23: Shifted from the CoinMarketCap ranking-JSON file to the
more-comprehensive listing-JSON file.

* 2021-03-09: Saving CoinMarketCap JSON files to the data-store:
http://logicalgraphs.blogspot.com/2021/03/progress-report-2021-03-09.html
