library(quantmod)
config::get()$alpha.vantage.key

setDefaults(getSymbols.av, api.key='XG3T5XUFFDLJ0JGY')

tickers<-c("AAPL", "MSFT", "SPY")

from.dat <- as.Date("01/01/2010", format="%d/%m/%Y")
to.dat <- as.Date("25/12/2011", format="%d/%m/%Y")

symbol<-tickers[1]

x<-getSymbols(Symbols='AAPL', src="av", output.size="full", output.size="full", adjusted=TRUE, api.key=config::get()$alpha.vantage.key)


