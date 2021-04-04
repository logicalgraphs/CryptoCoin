module CryptoCoin.CoinMarketCap.Analytics.Trends.OnBalanceVolume where


{--
Calculating OBV
On-balance volume provides a running total of an asset's trading volume and indicates whether this volume is flowing in or out of a given security or currency pair. The OBV is a cumulative total of volume (positive and negative). There are three rules implemented when calculating the OBV. They are:

1. If today's closing price is higher than yesterday's closing price, then: Current OBV = Previous OBV + today's volume


2. If today's closing price is lower than yesterday's closing price, then: Current OBV = Previous OBV - today's volume


3. If today's closing price equals yesterday's closing price, then: Current OBV = Previous OBV

Example Of How To Use On-Balance Volume
Below is a list of 10 days' worth of a hypothetical stock's closing price and volume:

Day one: closing price equals $10, volume equals 25,200 shares
Day two: closing price equals $10.15, volume equals 30,000 shares
Day three: closing price equals $10.17, volume equals 25,600 shares
Day four: closing price equals $10.13, volume equals 32,000 shares
Day five: closing price equals $10.11, volume equals 23,000 shares
Day six: closing price equals $10.15, volume equals 40,000 shares
Day seven: closing price equals $10.20, volume equals 36,000 shares
Day eight: closing price equals $10.20, volume equals 20,500 shares
Day nine: closing price equals $10.22, volume equals 23,000 shares
Day 10: closing price equals $10.21, volume equals 27,500 shares
As can be seen, days two, three, six, seven and nine are up days, so these trading volumes are added to the OBV. Days four, five and 10 are down days, so these trading volumes are subtracted from the OBV. On day eight, no changes are made to the OBV since the closing price did not change. Given the days, the OBV for each of the 10 days is:

Day one OBV = 0
Day two OBV = 0 + 30,000 = 30,000
Day three OBV = 30,000 + 25,600 = 55,600
Day four OBV = 55,600 - 32,000 = 23,600
Day five OBV = 23,600 - 23,000 = 600
Day six OBV = 600 + 40,000 = 40,600
Day seven OBV = 40,600 + 36,000 = 76,600
Day eight OBV = 76,600
Day nine OBV = 76,600 + 23,000 = 99,600
Day 10 OBV = 99,600 - 27,500 = 72,100

https://www.investopedia.com/terms/o/onbalancevolume.asp
--}
