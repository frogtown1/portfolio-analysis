#Description: Various portfolio optimization methods.



source(file = "00-core.R")


assetsMeanCov(rtns_log, method = "cov")

portfData <- rtns_log


##############################
###                        ###
###        Mean-Var        ###
###                        ###
##############################

# Global Minimum Variance
globminSpec <- portfolioSpec()

globminPortfolio <- minvariancePortfolio(
  data = portfData,
  spec = globminSpec,
  constraints = "LongOnly")
print(globminPortfolio)

# Tangency
tgSpec <- portfolioSpec()
setRiskFreeRate(tgSpec) <- 0

tgPortfolio <- tangencyPortfolio(
  data = portfData,
  spec = tgSpec,
  constraints = "LongOnly")
print(tgPortfolio)

# MCD Robustified Mean-Variance
covMcdEstimate <- covMcdEstimator(portfData)
fastCovMcdEstimator <- function(x, spec = NULL, ...)
  covMcdEstimate

covMcdSpec <- portfolioSpec()
setEstimator(covMcdSpec) <- "fastCovMcdEstimator"
setNFrontierPoints(covMcdSpec) <- 5
covMcdFrontier <- portfolioFrontier(
  data = portfData,
  spec = covMcdSpec)
print(covMcdFrontier)

# OGK Robustified Mean-Variance
covOGKEstimate <- covOGKEstimator(portfData)
fastCovOGKEstimator <- function(x, spec = NULL, ...) 
  covOGKEstimate

covOGKSpec <- portfolioSpec()
setEstimator(covOGKSpec) <- "fastCovOGKEstimator"
setNFrontierPoints(covOGKSpec) <- 25

covOGKFrontier <- portfolioFrontier(
  data = portfData,
  spec = covOGKSpec)
print(covOGKFrontier)


##############################
###                        ###
###        Mean-CVar       ###
###                        ###
##############################

# Feasible 'equal weight'
cvarSpec <- portfolioSpec()
setType(cvarSpec) <- "CVAR"
nAssets <- ncol(portfData)
setWeights(cvarSpec) <- rep(1/nAssets, times = nAssets)
setSolver(cvarSpec) <- "solveRglpk.CVAR"
ewPortfolio <- feasiblePortfolio(
  data = portfData,
  spec = cvarSpec,
  constraints = "LongOnly")
print(ewPortfolio)


# Feasible but with Lowest CVaR
minriskSpec <- portfolioSpec()
setType(minriskSpec) <- "CVaR"
setAlpha(minriskSpec) <- 0.05
setSolver(minriskSpec) <- "solveRglpk.CVAR"
setTargetReturn(minriskSpec) <- getTargetReturn(ewPortfolio@portfolio)["mean"]
minriskPortfolio <- efficientPortfolio(
  data = portfData,
  spec = minriskSpec,
  constraints = "LongOnly")
print(minriskPortfolio)

# Sortino Mean-CVar
ratioSpec <- portfolioSpec()
setType(ratioSpec) <- "CVaR"
setAlpha(ratioSpec) <- 0.05
setSolver(ratioSpec) <- "solveRglpk.CVAR"
setRiskFreeRate(ratioSpec) <- 0

ratioPortfolio <- maxratioPortfolio(
  data = portfData,
  spec = ratioSpec,
  constraints = "LongOnly")
print(ratioPortfolio)





library(dygraphs)
example1 <- cbind(globminPortfolio, tgPortfolio)

dygraph(example1[1,1])




weightsPie(globminPortfolio, box = FALSE, col = divPalette(ncol(portfData), "BrBG"))
mtext(text = "Global Minimum Variance MV Portfolio", side = 3,line = 1.5, font = 2, cex = 0.7, adj = 0)

getMean(globminPortfolio)
barplot(height = weightedReturns, names.arg = names, horiz = TRUE, las = 1, col = col)> title(main = "Weighted Portfolio Returns", xlab = "Weighted Returns %")

globmin_weights <- 100 * as.vector(getWeights(globminPortfolio))
globmin_weighted_returns <- globmin_weights * getMean(globminPortfolio)
barplot(height = globmin_weighted_returns, names.arg = colnames(portfData), horiz = TRUE, las = 1, col = divPalette(ncol(portfData), "BrBG"))
title(main = "Weighted Portfolio Returns", xlab = "Weighted Returns %")

covRiskBudgetsPie(globminPortfolio, box = FALSE, col = divPalette(ncol(portfData), "BrBG"))
mtext(text = "Global Minimum Variance MV Portfolio", side = 3, line = 1.5,font = 2, cex = 0.7, adj = 0)


col <- divPalette(ncol(portfData), "Spectral")
weightsPie(globminPortfolio, box = FALSE, col = col)
mtext(text = "Global Minimum Variance MV Portfolio", side = 3,line = 1.5, font = 2, cex = 0.7, adj = 0)
weightedReturnsPie(globminPortfolio, box = FALSE, col = col)
mtext(text = "Global Minimum Variance MV Portfolio", side = 3,line = 1.5, font = 2, cex = 0.7, adj = 0)
covRiskBudgetsPie(globminPortfolio, box = FALSE, col = col)
mtext(text = "Global Minimum Variance MV Portfolio", side = 3,line = 1.5, font = 2, cex = 0.7, adj = 0)



frontier <- portfolioFrontier(rtns_log)
tailoredFrontierPlot(frontier)
weightsPlot(frontier)


djiFrontier <- portfolioFrontier(portfData, covOGKSpec)
col = divPalette(15, "Spectral")
weightsPlot(djiFrontier, col = col)
