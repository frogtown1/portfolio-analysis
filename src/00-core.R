#Description: base scripts.


packages <- c('quantmod', 'PerformanceAnalytics', 'fPortfolio')
if (length(setdiff(packages,rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
invisible(lapply(packages, library, character.only = TRUE))


portfA <- new.env()


portfA$symbols <- c('SIVB', 'BK', 'AXP',
                    'GCRTX', 'PSP', 'QAI',
                    'BRK-B',
                    'GTLS',
                    'MT', 'IAU', 'PPLT',
                    'WOOD', 'PHO',
                    'VNQ',
                    'IVV')

# For each symbol, get adjusted prices by month from 2010-2020.
portfA$adj_prices_data <- NULL
for(symbol in portfA$symbols){
  portfA$adj_prices_data <- 
    cbind(portfA$adj_prices_data, 
          getSymbols(symbol, from = '2012-01-01', to = '2021-09-01',
                     periodicity = 'monthly', auto.assign = FALSE, index.class ='Date',
                     return.class = 'timeSeries')[,6])  
}
colnames(portfA$adj_prices_data) <- portfA$symbols

adj_prices <- cbind(
  portfA$adj_prices_data
  )

# Calculate log returns and their means and standard deviations.
rtns_log <- removeNA(round(Return.calculate(adj_prices, method = 'log'), 7))

summary(timeSeries(rtns_log))
