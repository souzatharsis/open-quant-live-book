#RiskParity.R
library(riskParityPortfolio)
library(IDPmisc)
library(fPortfolio)
library(xts)
library(purrr)
library(rlist)
# load FAANG returns
faang.returns<-as.xts(read.zoo('./data/FAANG.csv',
                         header=TRUE,
                         index.column=1, sep=","))


######### Portfolio Rebalancing


### Design

## Data
# FAANG Stock Returns

## Methodology
# Calculate quarterly weights for (i) parity portfolio; (ii) MWitz conservative; (iii) MWitz Agressive
# Plot weights as of today
# Calculate and discuss weights over time and performance
# Calculate cross-similarity matrix
# Do same but with mandates



######################## Calculate Risk Parity Portfolio vs MWitz as of 2018
faang.returns.filtered <- NaRV.omit(as.matrix(faang.returns["2018"]))
Sigma <- cov(faang.returns.filtered)

# compute risk parity portfolio
portfolio.parity <- riskParityPortfolio(Sigma)

# compute tangency portfolio
portfolio.tangency <- tangencyPortfolio(as.timeSeries(faang.returns.filtered), constraints = "LongOnly")

portfolio.weights <- rbind(portfolio.parity$w, getWeights(portfolio.tangency))
row.names(portfolio.weights)<-c("Parity Portfolio", "Tangency Portfolio")
# plot the portfolio designed
barplot(portfolio.weights, main = "Portfolio Weights", xlab = "stocks", ylab = "dollars",
        beside = TRUE, legend = TRUE, col=c("black", "red"),
        args.legend = list(bg = "white"))

portfolio.risks <- rbind(portfolio.parity$risk_contribution/sum(portfolio.parity$risk_contribution), getCovRiskBudgets(portfolio.tangency))
row.names(portfolio.risks)<-c("Parity Portfolio", "Tangency Portfolio")
barplot(portfolio.risks, main = "Portfolio Risk Budgets", xlab = "stocks", ylab = "Covariance Risk Budget",
        beside = TRUE, legend = TRUE, col=c("black", "red"),
        args.legend = list(bg = "white"))

######################## Calculate Risk Parity Portfolio over time
ApplyFilter <- function(from, to, R, FUN){
  return(FUN(R[paste0(from, "/", to)]))
}

ApplyRolling <- function(from, to, R, FUN){
    return(map2(from, to, ApplyFilter, R=R, FUN=FUN))
}

CalculateRiskParity <- function(r){
  return(riskParityPortfolio(cov(r))$w)
}

RollingRiskParity <- function(from, to, r){
  library(rlist)
  p<-ApplyRolling(from, to, r, CalculateRiskParity)
  names(p)<-to
  return(list.rbind(p))
}


faang.returns.xts<-faang.returns["2014-01-01/2019-09-01"]
rWindows<-rollingWindows(faang.returns.xts, period="12m",
                         by="3m")


parity.weights<-RollingRiskParity(rWindows$from@Data, rWindows$to@Data, faang.returns.xts)

######################## Calculate MWitz over time
faang.returns.ts<-as.timeSeries(faang.returns.xts)
Spec = portfolioSpec()

rolling.portfolio.tangency <- rollingTangencyPortfolio(faang.returns.ts,
                                                       constraints = "LongOnly",
                                                       from=rWindows$from,
                                                       to=rWindows$to,
                                                       spec=Spec)

names(rolling.portfolio.tangency)<-rWindows$to
tanweights <- sapply(rolling.portfolio.tangency,getWeights)
rownames(tanweights) <- colnames(faang.returns.ts)
tan.weights<-t(tanweights)
######################### Performance Analytics
library(PerformanceAnalytics)

### Weights per stock over time
chart.StackedBar(tan.weights)
chart.StackedBar(parity.weights)

### Calculate Weighted Returns
tan.returns <- Return.portfolio(faang.returns.xts, weights=tan.weights,verbose=TRUE)
parity.returns <- Return.portfolio(faang.returns.xts, weights=parity.weights,verbose=TRUE)
p.returns<-merge(tan.returns$returns, parity.returns$returns)
names(p.returns)<-c("tangency.portfolio", "parity.portfolio")

### Performance Summary (return / drawdown)
charts.PerformanceSummary(p.returns, colorset=rich6equal,
                          main = "Performance Summary: Tangency X Parity Portfolios",
                          lwd=2, cex.legend = 1.5, event.labels = TRUE)


### Calendar Returns
t(table.CalendarReturns(p.returns$tangency.portfolio))
t(table.CalendarReturns(p.returns$parity.portfolio))

### Rolling annualized return / sd / Sharpe Ratio
charts.RollingPerformance(p.returns, width = 252, colorset=rich6equal, event.labels = TRUE, legend.loc = "topleft")

### Summary stats across the period
table.AnnualizedReturns(p.returns)
