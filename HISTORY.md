# Revision History

* 2021-06-05: reporting the average price of coin bought.
* 2021-06-04: added transfers of funds between accounts. Transactions now 
  transfer funds from the linked-account, updating that account's balance.
* 2021-06-01: Updated reports: portfolii show even if 'next day' (UTC)
* 2021-05-29: Transaction reports now as CSV
* 2021-05-28: Reports now output as CSV
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
