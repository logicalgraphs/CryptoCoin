LE_DATE=$(date +%Y-%m-%d)

echo "Loading e-coin listing file for $LE_DATE ..."

LISTING_FILE=$CRYPTOCOIN_DIR/data-files/listings/listings-$LE_DATE.json

LIST_CMD="listings/latest?start=1&limit=5000&convert=USD"

CURL_CMD=$COIN_MARKET_CAP_DIR/scripts/curl-cmc.sh

$CURL_CMD cryptocurrency/$LIST_CMD $LISTING_FILE

$COIN_MARKET_CAP_DIR/scripts/run-load-listings.exp

echo "...done."
