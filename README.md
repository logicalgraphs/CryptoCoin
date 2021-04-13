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

## Patterns used

### Candlesticks

First, the [Five Most Powerful Candlestick Patterns](https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp), according to [investopedia](https://www.investopedia.com/).

* Three White Knights/Soldiers: https://www.investopedia.com/terms/t/three_white_soldiers.asp
* Three Black Crows: https://www.investopedia.com/terms/t/three_black_crows.asp
* Three Line Strike: https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#three-line-strike
* Abandoned Baby: https://www.investopedia.com/terms/b/bullish-abandoned-baby.asp
* Two Black Gapping: https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#two-black-gapping
* Evening Star: https://www.investopedia.com/articles/active-trading/092315/5-most-powerful-candlestick-patterns.asp#evening-star

### Price trends

* Simple Moving Average: https://www.investopedia.com/terms/s/sma.asp
* Exponential Moving Average: https://www.investopedia.com/terms/e/ema.asp
* Moving Average Convergence/Divergence: https://www.investopedia.com/terms/m/macd.asp
* On Balance Volume: https://www.investopedia.com/terms/o/onbalancevolume.asp
* Relative Strength Index: https://www.investopedia.com/terms/r/rsi.asp 

TODOs:

* compute all indicators for all tracked stocks
  * update and store trends
* complete candlestick computations
  * Three Black Crows
  * Three Line Strike
  * Abandoned Baby
  * Two Black Gapping
  * Evening Star
* offer recommendations on e-coins from price trends
* offer recommendations on e-coins from candlesticks
* record recommendations: recommendation (call), why, ecoin, day

* for the trends we need a computation phase that just returns a double
  and a recommendation phase that, well, makes the recommendation off the
  computed trends. Then we need to store the computations and recommendations.

* build predictive model based off of recommendations
* download FCAS scores: ... once we have cmc_id-to-flipside_crypto_uuid mapping
  * map FCAS coin uuids to cmc_ids

OTHER TODOs: 

* create a portfolio: positions held, money invested, current value
* create a transaction history (you know, for tax purposes)

* apply analytics against coins (done:
we're using the top-5 candlestick patterns and the top 5 price trend indicators)

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

* 2021-04-13: Finished On Balance Volume, switched to list-monad for
trend-processing.

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
