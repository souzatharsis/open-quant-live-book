library(quantmod)
if(!require(iex)){
  install_github("imanuelcostigan/iex")
  library(iex)
}

###################### ALPHA VANTAGE

config::get()$alpha.vantage.key

setDefaults(getSymbols.av, api.key='XG3T5XUFFDLJ0JGY')

tickers<-c("AAPL", "MSFT", "SPY")

from.dat <- as.Date("01/01/2010", format="%d/%m/%Y")
to.dat <- as.Date("25/12/2011", format="%d/%m/%Y")

symbol<-tickers[1]

x<-getSymbols(Symbols='AAPL', src="av", output.size="full", output.size="full", adjusted=TRUE, api.key=config::get()$alpha.vantage.key)


###################### IEX




######################## Quandl
# library(Quandl)
#
#
# Quandl.api_key('V2TuJHRGC5irmyAXBszA')
#
# from.dat <- as.Date("01/01/2010", format="%d/%m/%Y")
# to.dat <- as.Date("01/01/2018", format="%d/%m/%Y")
# ticker <- "AAPL"
# dat<-Quandl(ticker, start_date = from.dat, end_date = to.dat, type="xts", adjusted=TRUE)
#
# plot(dat$)
#
# https://www.quandl.com/api/v3/databases?api_key=V2TuJHRGC5irmyAXBszA&current_page=2


