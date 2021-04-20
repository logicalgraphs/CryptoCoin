
# the idea here to to run the recommendations from the stored trends and
# stored candlesticks

echo "Running recommendations."

# we'll run the price and volume recommendations here.

$COIN_MARKET_CAP_DIR/scripts/run-candlestick-patterns.exp

echo "Recommendations done."
