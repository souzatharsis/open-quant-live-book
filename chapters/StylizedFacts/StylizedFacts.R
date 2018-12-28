# StylizedFacts.R
###### Libs
library(quantmod)
library(scales)
###### Includes
source("./chapters/util.R")



# ##### Functions ---------------------------------------------------------

Log.Return<-function(x){
  res<-diff(log(x))
  return(res[-1])
}

Load.Prices <- function(symbol, from.dat, to.dat){
  prices<-getSymbols(symbol, auto.assign=FALSE, from = from.dat, to = to.dat)
  return(prices)
}


Plot.Prices <- function(prices, title="", axis.title.y=TRUE, colour="black"){

  prices<-as.data.frame(prices)
  prices[,"date"]<-as.Date(row.names(prices))
  names(prices)<-c("Open", "High", "Low", "Close", "Volume", "Adj.Close", "date")

  df.plot<- prices

  p.price<-ggplot(df.plot, aes(x= date, y=Adj.Close)) +
    geom_line( colour=colour ) +
    scale_x_date()+
    theme_classic()+
    xlab("Ano") +
    ylab("Preço Ajustado da Ação")+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1),#legend.justification=c(1,1),
          #legend.position=c(0.3, 0.4),
          # legend.position="top",
          axis.text.x  = element_text(size=20),axis.text.y  = element_text(size=20),
          axis.title.x = element_text(size=25),
          axis.title.y = element_text(size=25),
          legend.text = element_text(size=30),

          plot.title = element_text(size=30),
          legend.title = element_blank(),

          #panel.background = element_rect(fill = 'white', colour = 'black'),
          #panel.border = element_blank(),
          panel.grid.major =element_blank(),
          panel.grid.minor = element_blank())+
    ggtitle(title)

  if(!axis.title.y){
    p.price<-p.price + theme(axis.title.y  = element_blank())
  }

  return(p.price)

}



# Main --------------------------------------------------------------------




# Load Daily Prices from Yahoo Finance ------------------------------------

tickers<-c("AAPL", "MSFT", "SPY")

getSymbols(Symbols=tickers, src="av", output.size="full",
           adjusted=TRUE, api.key=config::get()$alpha.vantage.key)

PETR0.prices<-Load.Prices(tickers[1], from.dat, to.dat)
ITAU.prices<-Load.Prices(tickers[2], from.dat, to.dat)
IBOV.prices<-Load.Prices(tickers[3], from.dat, to.dat)




# Plot Prices -------------------------------------------------------------
p.PETR0<-Plot.Prices(AAPL, tickers[1], colour = colours[1])
p.ITAU<-Plot.Prices(ITAU.prices, tickers[2],axis.title.y=FALSE, colour = colours[2])


png("price-ts.png", width = 20, height = 8, units = 'in', res = 300)
multiplot(p.PETR0, p.ITAU, cols = 2)
dev.off()


# Plot Log-returns --------------------------------------------------------
prices.intersection <- merge.xts(AAPL,MSFT,join="inner")

S1.price.name<-paste0(tickers[1], ".Adjusted")
S1.returns<-Log.Return(prices.intersection[,S1.price.name])

S2.price.name<-paste0(tickers[2], ".Adjusted")
S2.returns<-Log.Return(prices.intersection[,S1.price.name])

price.returns<-c(S1.returns,  S2.returns)

date.returns<-index(prices.intersection)
date.returns<-rep(date.returns[-1], 2)


df.plot <- data.frame(date=date.returns,
                      returns = as.vector(price.returns),
                      stock = gl(n=2, k=length(price.returns)/2, labels = c(tickers[1], tickers[2])))

p.price<-ggplot(df.plot, aes(x= date, y=returns, group=stock, color=stock) ) +
  geom_line( ) +
  scale_x_date()+
  theme_classic()+
  xlab("Ano") +
  ylab("Log-retorno")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),legend.justification=c(1,1),
        legend.position=c(0.2, 1),
        axis.text.x  = element_text(size=20),axis.text.y  = element_text(size=20),
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        legend.text = element_text(size=30),
        plot.title = element_text(size=30),
        legend.title = element_blank(),
        panel.grid.major =element_blank(),
        panel.grid.minor = element_blank())+
  scale_colour_brewer(palette="Set1")

png("logretorno-ts.png", width = 20, height = 8, units = 'in', res = 300)
plot(p.price)
dev.off()



# Distribution ------------------------------------------------------------
library(plyr)

dat<-df.plot
cdat <- ddply(dat, "stock", summarise,
              return.mean=mean(returns),
              return.sd=sd(returns))

######### Fat tails
S1.dat<-dat[dat$stock==tickers[1],]
S1.cdat<-cdat[cdat$stock==tickers[1],]

gg <- ggplot(S1.dat, aes(x=returns))
gg <- gg + geom_histogram(colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg <- gg + stat_function(fun=dnorm,
                         color="red",
                         args=list(mean=S1.cdat$return.mean,
                                   sd=S1.cdat$return.sd))
gg<-gg+theme_classic()+
  xlab("Log-retorno") +
  ylab("Densidade")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position="none",
        axis.text.x  = element_text(size=20),axis.text.y  = element_text(size=20),
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        legend.text = element_text(size=30),

        plot.title = element_text(size=30),
        legend.title = element_blank(),
        panel.grid.major =element_blank(),
        panel.grid.minor = element_blank())+
  ggtitle(paste0("Histograma: Log-retorno ", tickers[1]))


######## QQ-Plot
gg2<-ggplot(S1.dat, aes(sample=returns))+stat_qq(size=5, alpha=1)+
  theme_classic()+
  ylab(paste0("Valor do Quantile Observado")) +
  xlab("Valor do Quantile Teórico (Distribuição Normal)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),aspect.ratio=1,
        axis.text.x  = element_text(size=20),axis.text.y  = element_text(size=20),
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        legend.text = element_text(size=30),

        plot.title = element_text(size=30),
        legend.position="none")+
  ggtitle(paste0("QQ-Plot: Log-retorno ", tickers[1]))



png("fat_tail.png", width = 20, height = 8, units = 'in', res = 300)
multiplot(gg,gg2, cols = 2)
dev.off()

# Volatility --------------------------------------------------------------
S2.dat<-dat[dat$stock==tickers[2],]
S2.cdat<-cdat[cdat$stock==tickers[2],]


############ Plot Top 50 returns
RetAbs<-abs(S2.dat$returns)
RetAbs100.index <- tail(order(RetAbs), 50)
RetAbs100<-RetAbs[RetAbs100.index]

Ret.Plot<-rep(0, length(RetAbs))
Ret.Plot[RetAbs100.index]<-RetAbs100


df.plot<- data.frame(abs.return=Ret.Plot, date=S1.dat$date)
title<-paste0("Clustering de Volatilidade: ", tickers[2])

p1<-ggplot(df.plot, aes(x= date, y=abs.return)) +
  geom_bar(stat="identity")+
  scale_x_date()+
  theme_classic()+
  xlab("Data") +
  ylab("Log-retorno: 50 maiores em valor absoluto")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),legend.position="none",
        axis.text.x  = element_text(size=20),axis.text.y  = element_text(size=20),
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        legend.text = element_text(size=30),

        plot.title = element_text(size=30))+
  ggtitle(title)

plot(p1)



################# Rolling Window
first.window <- unclass(S2.dat$date[1])
last.window <- unclass(S2.dat$date[length(S2.dat$date)])


window.size <- 21*3
window.step <- 5

date.int<-unclass(S2.dat$date)

window.list<-list()
for(date.seq in seq(from=first.window , to=last.window, by=window.step)){
  logreturns.window <- S2.dat[date.int %in% date.seq:(date.seq+window.size),]$returns
  window.list[[as.character(as.Date(date.seq))]] <- sd(logreturns.window)
}


window.list<-window.list[-length(window.list)]
df.plot<- data.frame(vol=unlist(window.list), date=as.Date(names(window.list)))

title<-paste0("Volatilidade no Tempo: ", tickers[2])

p2<-ggplot(df.plot, aes(x= date, y=vol)) +
  geom_line()+ #geom_point()+
  scale_x_date()+
  theme_classic()+
  xlab("Tempo") +
  ylab("Volatilidade")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),legend.position="none",
        axis.text.x  = element_text(size=20),axis.text.y  = element_text(size=20),
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        legend.text = element_text(size=30),

        plot.title = element_text(size=30))+
  ggtitle(title)


plot(p2)




png("vol_clustering.png", width = 20, height = 8, units = 'in', res = 300)
multiplot(p1, p2, cols = 2)
dev.off()




# Correlation -------------------------------------------------------------

### Load 8 stocks
tickers<-c("PETR4.SA", "PETR3.SA", "VALE3.SA", "VALE5.SA", "ITSA4.SA", "ITUB4.SA", "BBDC4.SA", "BBAS3.SA")

from.dat <- as.Date("01/01/2010", format="%d/%m/%Y")
to.dat <- as.Date("30/05/2016", format="%d/%m/%Y")

getSymbols(tickers, from = from.dat, to = to.dat)

current.ts<-get(tickers[1])[,6]
for(c in tickers[-1])
  current.ts<-merge.xts(current.ts, get(c)[,6], join="inner")

price.matrix<-as.data.frame(current.ts)
names(price.matrix)<-tickers

return.matrix<- sapply(price.matrix, Log.Return)


# Trading Volume

PETR4<-get("PETR4.SA")['2015-01::2016-04']

p1<-chartSeries(PETR4, theme='white', type='line')

vol.proxy<-(Hi(xts.S1)-Lo(xts.S1))/(Hi(xts.S1)+Lo(xts.S1))

volu.vola <- data.frame(volu=Vo(xts.S1), vola=vol.proxy)
names(volu.vola)<-c("volu", "vola")

p2<-ggplot(data=log(volu.vola) , aes(x=volu, y=vola))+
  geom_point(size=3)+
  theme_classic()+
  scale_x_continuous(name = "Volume de Negociação", labels = scales::math_format(10^.x)) +
  scale_y_continuous(name = "Volatilidade", labels = scales::math_format(10^.x)) +
  theme(legend.position="none",
        axis.text.x  = element_text(size=20),axis.text.y  = element_text(size=20),
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        legend.text = element_text(size=30),
        plot.title = element_text(size=25),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  ggtitle(paste0("Volume de Negociação", " X ", "Volatilidade"))+
  annotation_logticks(sides = "rtbl")
#annotate("text", x = -0.1, y = 0.1, label = paste0("corr = ", cor(x=return.matrix[, S1], y=return.matrix[, S2])))

png("ex-cor.png", width = 8, height = 8, units = 'in', res = 300)
plot(p1)
#,  cl.lim=c(0,1)
dev.off()

multiplot(p1, p2)


######## Correlation between 2 samples
S1<-tickers[1]
S2<-tickers[2]

p1<-ggplot(data=as.data.frame(return.matrix), aes(x=return.matrix[, S1], y=return.matrix[, S2]))+
  geom_point(size=3)+
  theme_classic()+
  xlab(paste0("Log-retorno: ", S1)) +
  ylab(paste0("Log-retorno: ", S2))+
  theme(legend.position="none",
        axis.text.x  = element_text(size=20),axis.text.y  = element_text(size=20),
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        legend.text = element_text(size=30),
        plot.title = element_text(size=30),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  ggtitle(paste0(S1, " X ", S2))
#annotate("text", x = -0.1, y = 0.1, label = paste0("corr = ", cor(x=return.matrix[, S1], y=return.matrix[, S2])))

png("ex-cor.png", width = 8, height = 8, units = 'in', res = 300)
plot(p1)
#,  cl.lim=c(0,1)
dev.off()

##### Correlation Matrix
library(corrplot)
M <- cor(return.matrix)
diag(M) = NA

col3 <- colorRampPalette(c("red", "white", "blue"))

png("cor.png", width = 8, height = 8, units = 'in', res = 300)
corrplot(M, order="hclust", addrect=2, col=col3(10), na.label = "x")
#,  cl.lim=c(0,1)
dev.off()


###### Cross correlation
library(forecast)

S1.l<-(tickers[5:6])
S2.l<-(tickers[7:8])
p.plot<-list()

for(S1 in S1.l)
  for(S2 in S2.l)
    p.plot[[paste0(S1, S2)]]<-ggCcf(as.data.frame(return.matrix[, S1]), as.data.frame(return.matrix[,S2]), lag.max=10)+
  theme_classic()+
  xlab("Lag") +
  ylab("Correlação Cruzada")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),legend.position="none",
        axis.text.x  = element_text(size=20),axis.text.y  = element_text(size=20),
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        legend.text = element_text(size=30),
        plot.title = element_text(size=30))+
  scale_x_continuous(breaks=seq(-10, 10, 1))+
  ggtitle(paste0(S1, " X ", S2))





png("cross-cor.png", width = 16, height = 16, units = 'in', res = 300)
multiplot(p.plot[[1]], p.plot[[2]], p.plot[[3]], p.plot[[4]], cols = 2)
dev.off()


###### Autocorrelacao
library(forecast)

S1<-(tickers[6])

lag.max<-20

p1 <- ggAcf(as.data.frame(return.matrix[, S1]), lag.max=lag.max)+
  theme_classic()+
  xlab("Lag") +
  ylab("Auto-correlação")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),legend.position="none",
        axis.text.x  = element_text(size=20),axis.text.y  = element_text(size=20),
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        legend.text = element_text(size=30),
        plot.title = element_text(size=30))+
  scale_x_continuous(breaks=seq(1, lag.max, 1))+
  ylim(-0.06, 0.12)+
  ggtitle(paste0("Log-retorno: ", S1))

p2 <- ggAcf(abs(as.data.frame(return.matrix[, S1])), lag.max=lag.max)+
  theme_classic()+
  xlab("Lag") +
  ylab("Auto-correlação")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),legend.position="none",
        axis.text.x  = element_text(size=20),axis.text.y  = element_text(size=20),
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        legend.text = element_text(size=30),
        plot.title = element_text(size=30))+
  scale_x_continuous(breaks=seq(1, lag.max, 1))+
  ylim(-0.06, 0.12)+
  ggtitle(paste0("Abs. Log-retorno: ", S1))





png("auto-cor.png", width = 16, height = 8, units = 'in', res = 300)
multiplot(p1, p2, cols = 2)
dev.off()




##### Correlation in time
S1.1<-tickers[1]
S1.2<-tickers[3]

first.window <-1
last.window <- length(return.matrix[,1])
dates<-index(get(tickers[1]))

window.size <- 21*3
window.step <- 5

return.df<-as.data.frame(return.matrix)

window.list<-list()
for(date.seq in seq(from=first.window , to=last.window, by=window.step)){
  logreturns.window.S1.1 <- return.df[date.seq:(date.seq+window.size),S1.1]
  logreturns.window.S1.2 <- return.df[date.seq:(date.seq+window.size),S1.2]

  window.list[[as.character(dates[date.seq])]] <- cor(logreturns.window.S1.1, logreturns.window.S1.2)

}


window.list<-window.list[-length(window.list)]
df.plot<- data.frame(vol=unlist(window.list), date=as.Date(names(window.list)))

title<-paste0(S1.1, " X ", S1.2)

p2<-ggplot(df.plot, aes(x= date, y=vol)) +
  geom_line()+ #geom_point()+
  scale_x_date()+
  theme_classic()+
  xlab("Tempo") +
  ylab("Correlação")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),legend.position="none",
        axis.text.x  = element_text(size=20),axis.text.y  = element_text(size=20),
        axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25),
        legend.text = element_text(size=30),

        plot.title = element_text(size=30))+
  ggtitle(title)




png("cor_temporal.png", width = 20, height = 8, units = 'in', res = 300)
plot(p2)
dev.off()

