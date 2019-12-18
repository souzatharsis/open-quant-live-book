source("chapters/util.R")

###### Nonlinear Coupling
library(ggplot2)

LambdaCorrelation <- function(mutual_info){
  return(sqrt(1-exp(-2*mutual_info)))
}

SturgesBin <- function(num.points){
  return(ceiling(log(num.points,2)+1))
}

MutualInformation <- function(cause, effect){
  library(entropy)

  cause_numBins<- SturgesBin(length(cause))
  effect_numBins<- SturgesBin(length(effect))

  cause_effect_joint_disc <- discretize2d(cause, effect, numBins1=cause_numBins, numBins2=effect_numBins)

  #computes the mutual information of two discrete random variables from the specified joint bin frequencies
  mutualInfo <- mi.plugin(cause_effect_joint_disc)

  return(mutualInfo)
}

SampleMI <- function(n, MI.Func){

  # random effect
  effect <- runif(n)
  cause <- runif(n)

  return(MI.Func(cause, effect))

}


EffectiveMutualInformation <- function(cause, effect, MI.Func, sample.size=100){

  emi <- MI.Func(cause, effect) - mean(map_dbl(rep(length(cause), sample.size), SampleMI, MI.Func))

  emi <- ifelse(emi<0, 0, emi)

  return(emi)

}


#### Sample synthetic systems

## random
GetRandomSystem <- function(n){
  set.seed(8192)
  effect <- runif(n)
  set.seed(192)
  cause <- runif(n)

  return(list(cause=cause, effect=effect))
}

## linear
GetLinearSystem <- function(n, a=2, b=1){
  cause <- 1:n
  effect <- a*(cause) + b*rnorm(n, mean = mean(cause), sd = sd(cause))

  return(list(cause=cause, effect=effect))
}

## sin
GetSinSystem <- function(n){
  set.seed(8192)
  ii<-runif(n)
  cause<-sin(2*pi*ii)
  effect<-cos(2*pi*ii)

  return(list(cause=cause, effect=effect))
}

## loglinear
GetLogLinSystem <- function(n, a=1, b=3){
  cause <- 1:n
  effect <- a*log(cause) + b

  return(list(cause=cause, effect=effect))
}

## Mutual Information-based correlation
Correlation.MI<- function(size,s.system){
  EffectiveMutualInformation(s.system(size)$cause, s.system(size)$effect, MutualInformation) %>%
    LambdaCorrelation
}

## Linear correlation
Correlation.Linear <- function(size,s.system){
  cor(s.system(size)$cause, s.system(size)$effect)
}



########### Main

l.systems<-c(random=GetRandomSystem,
             linear=GetLinearSystem,
             sincos=GetSinSystem,
             log=GetLogLinSystem)

size<-10000
ts.sample.sizes<-seq(from=500, to=size, by=500)

MI.res<-outer(ts.sample.sizes, l.systems, Vectorize(Correlation.MI))
cor.res<-outer(ts.sample.sizes, l.systems, Vectorize(Correlation.Linear))


#
# for(n in seq(from=100, to=size, by=100)){
#
#   # random effect
#   set.seed(8192)
#   effect <- runif(n)
#   set.seed(192)
#   cause <- runif(n)
#
#   random.MI[i]<-EffectiveMutualInformation(cause, effect, MutualInformation)
#   random.cor[i]<-cor(cause, effect)
#
#   # linear effect + noise
#   cause <- 1:n
#   effect <- 2*(cause) + rnorm(n, mean = mean(cause), sd = sd(cause))
#
#   linear.MI[i]<-EffectiveMutualInformation(cause, effect, MutualInformation)
#   linear.cor[i]<-cor(cause, effect)
#
#   # sin cos
#   ii<-runif(n)
#   cause<-sin(2*pi*ii)
#   effect<-cos(2*pi*ii)
#
#   sin.MI[i]<-EffectiveMutualInformation(cause, effect, MutualInformation)
#   sin.cor[i]<-cor(cause, effect)
#
#   # log effect
#   cause <- 1:n
#   effect <- log(cause) + 3
#
#   log.MI[i] <- EffectiveMutualInformation(cause, effect, MutualInformation)
#   log.cor[i]<- cor(cause, effect)
#
#   i<-i+1
# }
#
#
# x<-data.frame(function.method=rep("log.MI",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(log.MI))
# #y<-data.frame(function.method=rep("log.Ker",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(log.KMI))
# z<-data.frame(function.method=rep("log.cor",size),num.points=seq(from=100, to=size, by=100), MI=log.cor)
#
# data.MI<-rbind(x,z)
#
# p44<-ggplot(data=data.MI, aes(x=num.points, y=MI, group=function.method, colour=function.method)) +
#   geom_line()+
#   geom_point()
#
# x<-data.frame(function.method=rep("sin.MI",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(sin.MI))
# #y<-data.frame(function.method=rep("sin.Ker",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(sin.KMI))
# z<-data.frame(function.method=rep("sin.cor",size),num.points=seq(from=100, to=size, by=100), MI=sin.cor)
#
# data.MI<-rbind(x,z)
#
# p22<-ggplot(data=data.MI, aes(x=num.points, y=MI, group=function.method, colour=function.method)) +
#   geom_line() +
#   geom_point()
#
# x<-data.frame(function.method=rep("lin.MI",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(linear.MI))
# #y<-data.frame(function.method=rep("lin.Ker",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(linear.KMI))
# z<-data.frame(function.method=rep("lin.cor",size),num.points=seq(from=100, to=size, by=100), MI=linear.cor)
#
# data.MI<-rbind(x,z)
#
# p33<-ggplot(data=data.MI, aes(x=num.points, y=MI, group=function.method, colour=function.method)) +
#   geom_line() +
#   geom_point()
#
#
# x<-data.frame(function.method=rep("rand.MI",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(random.MI))
# #y<-data.frame(function.method=rep("rand.Ker",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(random.KMI))
# z<-data.frame(function.method=rep("rand.cor",size),num.points=seq(from=100, to=size, by=100), MI=random.cor)
#
# data.MI<-rbind(x,z)
#
# p11<-ggplot(data=data.MI, aes(x=num.points, y=MI, group=function.method, colour=function.method)) +
#   geom_line() +
#   geom_point()
#
#
#
#
# # random effect
# set.seed(8192)
# effect <- runif(n)
# set.seed(192)
# cause <- runif(n)
#
# myplot<-data.frame(cause,effect)
#
# p1<-ggplot(data=myplot, aes(x=cause, y=effect)) +
#   geom_point()
#
#
#
#
#
# ii<-runif(n)
# cause<-sin(2*pi*ii)
# effect<-cos(2*pi*ii)
#
# myplot<-data.frame(cause,effect)
#
# p2<-ggplot(data=myplot, aes(x=cause, y=effect)) +
#   geom_point()
#
#
#
#
# cause <- 1:n
# effect <- 2*(cause) + rnorm(n, mean = mean(cause), sd = sd(cause))
#
# myplot<-data.frame(cause,effect)
#
# p3<-ggplot(data=myplot, aes(x=cause, y=effect)) +
#   geom_point()
#
#
#
# cause <- 1:n
# effect <- log(cause) + 3
#
# myplot<-data.frame(cause,effect)
#
# p4<-ggplot(data=myplot, aes(x=cause, y=effect)) +
#   geom_point()
#
#
# multiplot(p1,p2,p3,p4,p11,p22,p33,p44,cols=2)
#
#
#
#
#
#
#
#
#
#
#
# #####################
# Entropy.Hist<-function(x){
#   return(entropy(hist(x, plot=FALSE, breaks=20)$counts))
# }
#
#
#
#
# ########## Load SPY returns
# global.indices<-read.csv("data/global_indices_returns.csv")
# SPY <- global.indices
#
#
# ############## ENTROPY
# library(zoo)
# ## Calculate 252-rolling window entropy
# SPY$SPY.entropy<-rollapply(SPY$X.GSPC, width = 252, by = 21, FUN = Entropy.Hist, align = "right")
# ## Plot
# plot(SPY[!is.na(SPY$SPY.entropy),c("SPY.entropy")], main="SPY Daily Return 252-day Entropy")
#
