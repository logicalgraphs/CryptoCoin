LE_DATE=$(date +%Y-%m-%d)

LISTING_FILE=$CRYPTOCOIN_DIR/data-files/listings/listings-$LE_DATE.json

LIST_CMD="listings/latest?start=1&limit=5000&convert=USD"

CURL_CMD=$COIN_MARKET_CAP_DIR/scripts/curl-command.sh

$CURL_CMD cryptocurrency/$LIST_CMD $LISTING_FILE

cd $CRYPTOCOIN_DIR
$COIN_MARKET_CAP_DIR/scripts/run-report.exp

echo "done."
