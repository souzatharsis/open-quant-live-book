#RiskParity.R

library(riskParityPortfolio)

# generate synthetic data
set.seed(42)
N <- 10
V <- matrix(rnorm(N^2), nrow = N)
Sigma <- cov(V)

# compute risk parity portfolio
portfolio <- riskParityPortfolio(Sigma)

# plot the portfolio designed
barplot(portfolio$w, main = "Portfolio Weights", xlab = "stocks", ylab = "dollars",
        beside = TRUE, col = rainbow8equal[1], legend = c("riskParityPortfolio"),
        args.legend = list(bg = "white"))



barplot(portfolio$w, main = "Portfolio Weights", xlab = "stocks", ylab = "dollars",
        beside = TRUE, col = "red", legend = c("riskParityPortfolio"),
        args.legend = list(bg = "white"))
