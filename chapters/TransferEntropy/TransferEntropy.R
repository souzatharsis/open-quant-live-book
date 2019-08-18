# TransferEntropy.R
library(IDPmisc)
library(quantmod)
library(RTransferEntropy)
library(future)

### Apply function D.Func to all pairwise combinations of the data.frame X
### Returns a matrix: output[a,b] = D.Func(X[,a], X[,b])
FApply.Pairwise <- function(X, D.Func){
  n = seq_len(ncol(X))

  ff.TE.value = function(a, b) calc_te(X[,a], X[,b])

  return(outer(n, n, Vectorize(ff.TE.value)))
}

#https://finance.yahoo.com/world-indices/
tickers<-c("^GSPC", "^FTSE", "^GDAXI", "^N100", "^BVSP")

data.env <- new.env()
dataset<- xts() # Only run once


# Download prices from AlphaVantage and calculate log-returns
for(i in 1:length(tickers)) {
  tickers[i]-> symbol
  print(symbol)
  getSymbols(symbol, src="av",
             auto.assign=TRUE,
             output.size="full",
             adjusted=TRUE,
             api.key=config::get()$alpha.vantage.key)

    dataset <- merge(dataset, periodReturn(Ad(get(tickers[i])),period="daily", type='log'))
    rm(symbol)
}

names(dataset)<-tickers
write.csv(dataset, file="./data/global_indices_returns.csv", row.names = TRUE, )
tmp <- tempfile()
write.zoo(dataset,sep=",",file="./data/global_indices_returns.csv")

dataset.post.crisis <- NaRV.omit(as.data.frame(dataset["2014-01-01/"]))


## Allow for parallel computing
plan(multiprocess)
# Calculate pairwise Transfer Entropy among global indices
TE.matrix<-FApply.Pairwise(dataset.post.crisis, calc_ete())
rownames(TE.matrix)<-colnames(TE.matrix)<-tickers

corrplot::corrplot(corr = TE.matrix/max(TE.matrix), diag = FALSE, order = "hclust", is.corr = FALSE)


dataset2<-read.zoo('./data/global_indices_returns.csv',
                  header=TRUE,
                  index.column=1, sep=",")
