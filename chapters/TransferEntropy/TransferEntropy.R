# TransferEntropy.R
library(IDPmisc)
library(quantmod)
library(RTransferEntropy)
library(future)

### Apply function D.Func to all pairwise combinations of the data.frame X
### Returns a matrix: output[a,b] = D.Func(X[,a], X[,b])
FApply.Pairwise <- function(X, D.Func){
  n = seq_len(ncol(X))

  ff.TE.value = function(a, b) D.Func(X[,a], X[,b])

  return(outer(n, n, Vectorize(ff.TE.value)))
}


Linear.GC <- function(X, Y){

  n<-length(X)
  X.now<-X[1:(n-1)]
  Y.now<-Y[1:(n-1)]
  Y.fut<-Y[2:n]

  regression.uni=lm(Y.fut~Y.now)
  regression.mult=lm(Y.fut~Y.now+ X.now)
  var.eps.uni <- (summary(regression.uni)$sigma)^2
  var.eps.mult <- (summary(regression.mult)$sigma)^2
  GC <- log(var.eps.uni/var.eps.mult)
  return(GC)
}




######## Simulated systems

n.points<-10000
x1<-x2<-x3<-x4<-x5<-vector()
x1[1]<-x2[1]<-x3[1]<-x4[1]<-x5[1]<-0
w1<-rnorm(n.points,0,1)
w2<-rnorm(n.points,0,1)
w3<-rnorm(n.points,0,1)
w4<-rnorm(n.points,0,1)
w5<-rnorm(n.points,0,1)
################ LINEAR
for(i in 2:(n.points)){
  x1[i] <- 0.95*sqrt(2)*x1[i-1] - 0.9025*x1[i-1] + w1[i]
  x2[i] <- 0.5*(x1[i-1]) + w2[i]
  x3[i] <- -0.4*x1[i-1] + w3[i]
  x4[i] <- -0.5*(x1[i-1]) + 0.25*sqrt(2)*x4[i-1] + 0.25*sqrt(2)*x5[i-1] + w4[i]
  x5[i] <- -0.25*sqrt(2)*(x4[i-1]) + 0.25*sqrt(2)*x5[i-1] + w5[i]
}
x1 <- x1[-1]
x2 <- x2[-1]
x3 <- x3[-1]
x4 <- x4[-1]
x5 <- x5[-1]

linear.system <- data.frame(x1, x2, x3, x4, x5)

TE.matrix1<-FApply.Pairwise(linear.system, calc_te)
TE.matrix2<-FApply.Pairwise(linear.system, Linear.GC)
rownames(TE.matrix1)<-colnames(TE.matrix1)<-var.names<-c("x1", "x2", "x3", "x4", "x5")
rownames(TE.matrix2)<-colnames(TE.matrix2)<-var.names<-c("x1", "x2", "x3", "x4", "x5")
corrplot::corrplot(corr = TE.matrix1, diag = FALSE,
                   is.corr = FALSE, outline = T,tl.col = "black",
                   method = "color",
                   col = colorRampPalette(c("blue","white", "red"))(100))

corrplot::corrplot(corr = TE.matrix2, diag = FALSE,
                   is.corr = FALSE, outline = T,tl.col = "black",
                   method = "color",
                   col = colorRampPalette(c("blue","white", "red"))(100))

library(future)
## Allow for parallel computing
plan(multiprocess)
transfer_entropy(x4, x5)

###### Global Market Indices

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
