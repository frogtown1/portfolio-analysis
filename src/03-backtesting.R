#Description: backtest portfolio methods from 02.



source(file = "00-core.R")
source(file = "02-portf-optimizations.R")


charts.PerformanceSummary(rtns_log)


colnames(rtns_log)
spiData <- rtns_log
spiSpec <- portfolioSpec()
spiConstraints <- "LongOnly"
spiBacktest <- portfolioBacktest()

spiFormula <- IVV ~ AAPL + AMZN + COST + GLW + GCRTX + BRK-B + PSP + EDV + QAI + ZROZ + IAU + MARA + MT + USO + PHO + WOOD + GLD + IVV

spiPortfolios <- portfolioBacktesting(formula = spiFormula,data = spiData, spec = spiSpec, constraints = spiConstraints,backtest = spiBacktest, trace = FALSE)


library(dygraphs)



tg_weights
covOGK_weights
sortino_ratio_weights




# Compute Price Weighted Index

Price_Weights <- close_prices[,1:5] / rowSums(close_prices[,1:5])
Price_Weighted_Index <- rowSums(returns_stocks[,1:5] * Price_Weights)
# Track a $10K investment in the index
Price_Weighted_Index[1] <- 10000
for (i in 2:length(Price_Weighted_Index)) {
  Price_Weighted_Index[i] <- Price_Weighted_Index[i-1] + (Price_Weighted_Index[i] * Price_Weighted_Index[i-1])
}
fin_6310$PW_Index <- Price_Weighted_Index
# Compute the SPY index of a similar $10K investment.
fin_6310$SPY_Index <- returns_stocks$SPY.Adjusted
fin_6310$SPY_Index[1,] <- 10000    
SPY_Index <- as.numeric(fin_6310$SPY_Index)
for (i in 2:length(SPY_Index)) {
  SPY_Index[i] <- SPY_Index[i-1] + (SPY_Index[i] * SPY_Index[i-1])
}
fin_6310$SPY_Index <- SPY_Index
# Combine the two index investments and plot
index_frame <- fin_6310$SPY$SPY.Adjusted[-1,]
index_frame$SPY_Index <- SPY_Index
index_frame$PW_Index <- Price_Weighted_Index
index_frame <- index_frame[,-1]
dygraph(index_frame, main = "Performance of $10K Investment in SPY and Price-Weighred Index")







#
#     Performance of $10K in IVV & Price-weighted Index
#
library(dygraphs)
#Prep price-weighted index
Price_Weights <- prices[,1:17] / rowSums(prices[,1:17])
Price_Weighted_Index <- rowSums(rtns_log[,1:17] * Price_Weights)
# Track a $10K investment in the index
Price_Weighted_Index[1] <- 10000
for (i in 2:length(Price_Weighted_Index)) {
  Price_Weighted_Index[i] <- Price_Weighted_Index[i-1] + (Price_Weighted_Index[i] * Price_Weighted_Index[i-1])
}
pw_index <- Price_Weighted_Index
#Prep benchmark(IVV) index
IVV_index <- rtns_log$IVV
IVV_index[1,] <- 10000    
IVV_index <- as.numeric(IVV_index)
for (i in 2:length(IVV_index)) {
  IVV_index[i] <- IVV_index[i-1] + (IVV_index[i] * IVV_index[i-1])
}
# Combine the two index investments and plot
index_frame <- prices$IVV
index_frame$IVV_Index <- IVV_index
index_frame$PW_Index <- pw_index
index_frame <- index_frame[,-1]
#head(index_frame)
#tail(index_frame)

dygraph(index_frame, main = "Performance of $10K Investment in IVV and Price-Weighted Index")