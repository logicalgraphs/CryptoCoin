# wow. Makefile. Wow. We went there.

SCRIPTS_DIR=$(COIN_MARKET_CAP_DIR)/scripts

# we intentionally disconnect the (non-)dependency between loaders and reports
# so you can run reports without loading data.

ecoins: loaders reports
	@echo "ecoins: didst runneth."

EXCLUDE=-path ./src/Data/BlockChain -prune -false -o
VERIFIER=$(SCRIPTS_DIR)/verify-haskell-file.exp
ERR_FILE=errs.txt

# the programs are the things we run from scripts directory. How do we 
# macrotize this, I wonder?

# for now I run: ls $COIN_MARKET_CAP_DIR/scripts/*.* | xargs grep hs
# then generate these program inodes by hand.

PROGRAMS=$(COIN_MARKET_CAP_DIR)/ETL/Candlesticks/Loader.hs \
         $(COIN_MARKET_CAP_DIR)/ETL/SourceFileLoader.hs \
         $(COIN_MARKET_CAP_DIR)/ETL/TrackedCoinLoader.hs \
         $(COIN_MARKET_CAP_DIR)/ETL/TransactionCSVLoader.hs \
         $(COIN_MARKET_CAP_DIR)/ETL/TransferCSVLoader.hs \
         $(COIN_MARKET_CAP_DIR)/ETL/Coins/Transformer.hs \
         $(COIN_MARKET_CAP_DIR)/ETL/Candlesticks/Transformer.hs \
         $(COIN_MARKET_CAP_DIR)/Analytics/Candlesticks/Patterns.hs \
         $(COIN_MARKET_CAP_DIR)/Analytics/Trends/Recommendation.hs \
         $(COIN_MARKET_CAP_DIR)/Analytics/Trends/Trend.hs \
         $(COIN_MARKET_CAP_DIR)/Reports/Portfolio.hs \
         $(COIN_MARKET_CAP_DIR)/Reports/Recommendation.hs \
         $(COIN_MARKET_CAP_DIR)/Reports/Transaction.hs

# ----- ONE-OFFS ---------------------------------------------------------

verify: verify1 FORCE
	grep error $(ERR_FILE) || [[ $$? == 1 ]]
	rm $(ERR_FILE)

verify1: FORCE
	$(foreach file, $(PROGRAMS), $(VERIFIER) $(file) >> $(ERR_FILE);)

# ----- We want transactions to be independent, because we buy and sell
# ----- after the system makes the recommendations.

LOAD_TRANSACTIONS=$(SCRIPTS_DIR)/run-load-transactions.exp
REPORT_TRANSACTIONS=$(SCRIPTS_DIR)/run-report-transactions.exp

transactions: FORCE
	$(LOAD_TRANSACTIONS)
	$(REPORT_TRANSACTIONS)

# ----- Same for transfers (transfers show up on portfolii in that report)

LOAD_TRANSFERS=$(SCRIPTS_DIR)/run-load-transfers.exp

transfers: FORCE
	$(LOAD_TRANSFERS)

# ----- Same for staking reinvestments

LOAD_REINVESTMENTS=$(SCRIPTS_DIR)/run-load-reinvestments.exp

reinvest: FORCE
	$(LOAD_REINVESTMENTS)

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

RECOMMENDATION_REPORT=$(SCRIPTS_DIR)/run-report-recommendations.exp

reports: recommendations
	@echo "Ran the reports"

recommendations: $(RECOMMENDATION_REPORT)
	true

REC_REPORT_DEPENDS=$(COIN_PROCESSOR) \
                   $(TREND_PATTERNS) \
                   $(CANDLESTICK_PATTERNS)

$(RECOMMENDATION_REPORT): $(REC_REPORT_DEPENDS) FORCE
	$(RECOMMENDATION_REPORT)

FORCE:
