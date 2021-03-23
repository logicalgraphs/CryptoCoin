LE_DATE=$(date +%Y-%m-%d)

LISTING_FILE=$COIN_MARKET_CAP_DIR/listings/listings-$LE_DATE.json

LIST_CMD="listings/latest?start=1&limit=5000&convert=USD"

CURL_CMD=$COIN_MARKET_CAP_DIR/scripts/curl-command.sh

cd $COIN_MARKET_CAP_DIR/scripts
$CURL_CMD cryptocurrency/$LIST_CMD $LISTING_FILE

runhaskell $COIN_MARKET_CAP_DIR/scripts/report.hs

echo "done."
