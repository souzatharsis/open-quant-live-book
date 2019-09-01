library(quantmod)
if(!require(iex)){
  install_github("imanuelcostigan/iex")
  library(iex)
}

###################### ALPHA VANTAGE
setDefaults(getSymbols.av, api.key=config::get()$alpha.vantage.key)

tickers<-c("AAPL", "MSFT", "SPY")

from.dat <- as.Date("01/01/2010", format="%d/%m/%Y")
to.dat <- as.Date("25/12/2011", format="%d/%m/%Y")

symbol<-tickers[1]

x<-getSymbols(Symbols='AAPL', src="av", output.size="full", output.size="full", adjusted=TRUE, api.key=config::get()$alpha.vantage.key)


###################### IEX




######################## Quandl
library(Quandl)
Quandl.api_key(config::get()$quandl.key)
from.dat <- as.Date("01/01/2010", format="%d/%m/%Y")
to.dat <- as.Date("01/01/2019", format="%d/%m/%Y")
crude.oil.futures<-Quandl("CHRIS/CME_CL1", start_date = from.dat, end_date = to.dat, type="xts")
plot(crude.oil.futures$Last)

xx<-Quandl.datatable("ZACKS/MKTV", per_end_date='2008-09-30', ticker="MSFT", paginate = TRUE)

