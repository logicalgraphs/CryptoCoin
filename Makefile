# wow. Makefile. Wow. We went there.

SCRIPTS_DIR=$(COIN_MARKET_CAP_DIR)/scripts

# we intentionally disconnect the (non-)dependency between loaders and reports
# so you can run reports without loading data.

ecoins: loaders reports
	@echo "ecoins: didst runneth."

EXCLUDE=-path ./src/Data/BlockChain -prune -false -o
VERIFIER=$(SCRIPTS_DIR)/verify-haskell-file.exp
ERR_FILE=errs.txt

# ----- ONE-OFFS ---------------------------------------------------------

verify: verify1 FORCE
	grep error $(ERR_FILE)
	rm $(ERR_FILE)

verify1: FORCE
	find . $(EXCLUDE) -name "*.hs" -exec $(VERIFIER) {} \; > $(ERR_FILE)

# ----- We want transactions to be independent, because we buy and sell
# ----- after the system makes the recommendations.

LOAD_TRANSACTIONS=$(SCRIPTS_DIR)/run-load-transactions.exp
REPORT_TRANSACTIONS=$(SCRIPTS_DIR)/run-report-transactions.exp

transactions: FORCE
	$(LOAD_TRANSACTIONS)
	$(REPORT_TRANSACTIONS)

PORTFOLIO_REPORT=$(SCRIPTS_DIR)/run-report-portfolii.exp

portfolio: FORCE
	$(PORTFOLIO_REPORT)

# ----- LOADER AND REPORT DEPENDENCIES -----------------------------------

CANDLESTICK_LOADER=$(SCRIPTS_DIR)/run-load-candlesticks.exp
TRACKED_COIN_LOADER=$(SCRIPTS_DIR)/run-load-tracked-coins.exp
LISTINGS_LOADER=$(SCRIPTS_DIR)/run-load-listings.sh 
LOADERS=$(LISTINGS_LOADER) $(CANDLESTICK_LOADER)

loaders: $(LOADERS)
	@echo "Ran loaders"

$(CANDLESTICK_LOADER): $(TRACKED_COIN_LOADER) FORCE   # d-mn-t! :<
	$(CANDLESTICK_LOADER)

$(TRACKED_COIN_LOADER): FORCE
	$(TRACKED_COIN_LOADER)

$(LISTINGS_LOADER): FORCE
	$(LISTINGS_LOADER)

CANDLESTICK_PROCESSOR=$(SCRIPTS_DIR)/run-process-candlesticks.exp
COIN_PROCESSOR=$(SCRIPTS_DIR)/run-process-coins.exp
TREND_PROCESSOR=$(SCRIPTS_DIR)/run-process-trends.exp

$(TREND_PROCESSOR): $(COIN_PROCESSOR) FORCE
	$(TREND_PROCESSOR)

$(COIN_PROCESSOR): FORCE
	$(COIN_PROCESSOR)

$(CANDLESTICK_PROCESSOR): FORCE
	$(CANDLESTICK_PROCESSOR)

CANDLESTICK_PATTERNS=$(SCRIPTS_DIR)/run-patterns-candlesticks.exp
TREND_PATTERNS=$(SCRIPTS_DIR)/run-patterns-trends.exp
PATTERNS=$(CANDLESTICK_PATTERNS) $(TREND_PATTERNS)

# patterns also save the recommendations from running the patterns

$(CANDLESTICK_PATTERNS): $(CANDLESTICK_PROCESSOR) FORCE
	$(CANDLESTICK_PATTERNS)

$(TREND_PATTERNS): $(TREND_PROCESSOR) FORCE
	$(TREND_PATTERNS)

COIN_REPORT=$(SCRIPTS_DIR)/run-report-coin.exp
RECOMMENDATION_REPORT=$(SCRIPTS_DIR)/run-report-recommendations.exp

reports: coin_reports recommendations
	@echo "Ran the reports"

coin_reports: $(COIN_REPORT)
	true

recommendations: $(RECOMMENDATION_REPORT)
	true

$(COIN_REPORT): $(COIN_PROCESSOR) FORCE
	$(COIN_REPORT)

$(RECOMMENDATION_REPORT): $(TREND_PATTERNS) $(CANDLESTICK_PATTERNS) FORCE
	$(RECOMMENDATION_REPORT)

FORCE:
