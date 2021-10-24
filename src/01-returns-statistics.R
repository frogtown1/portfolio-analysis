#Description: Exploring portfolio returns data.



source(file = "00-core.R")




seriesPlot(rtns_log)

histPlot(rtns_log)
densityPlot(rtns_log)
logDensityPlot(rtns_log)

chart.Boxplot(rtns_log, sort.by = 'variance', colorset = 'black')
boxPercentilePlot(rtns_log)

plot(assetsSelect(rtns_log, method = "hclust"), main = "Cluster Dendrogram", sub = "by Monthly Returns", xlab = "Portfolio Assets", ylab = "Distance (Euclidean Units)")

assetsCorEigenPlot(rtns_log, method = "kendall")
assetsCorImagePlot(rtns_log, use = 'kendall')


SkewnessKurtosisRatio(rtns_log)
SemiDeviation(rtns_log)
VaR(rtns_log, p = 0.025)
VaR(rtns_log, p = 0.05)
CVaR(rtns_log)
cvarRisk(rtns_log[1:19], weights = rep(c(1),19)/19, alpha = 0.05)
ES(rtns_log, p = 0.025)
ES(rtns_log, p = 0.05)


table.Drawdowns(rtns_log)

colQuantiles(rtns_log, prob = 0.05, type = 1)
basicStats(rtns_log)
table.AnnualizedReturns(rtns_log)
table.DrawdownsRatio(rtns_log)




shapiroTest <- assetsTest(rtns_log[-18], method = "shapiro")
print(shapiroTest)

assetsTest(rtns_log[-18], method = "energy")

assetsFit(rtns_log, method = "st")



rtns_mu <- round(mean.geometric(rtns_log), 4)
rtns_sigma <- round(sd.annualized(rtns_log), 4)
rtns_cov <- round(cov(rtns_log), 4)