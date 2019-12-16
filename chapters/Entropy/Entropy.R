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


random.MI<-vector()
random.cor<-vector()

linear.MI<-vector()
linear.cor<-vector()

sin.MI<-vector()
sin.cor<-vector()

log.MI<-vector()
log.cor<-vector()

i<-1
size<-4000

for(n in seq(from=100, to=size, by=100)){

  # random effect
  set.seed(8192)
  effect <- runif(n)
  set.seed(192)
  cause <- runif(n)

  random.MI[i]<-MutualInformation(cause, effect)
  random.cor[i]<-cor(cause, effect)

  # linear effect + noise
  cause <- 1:n
  effect <- 2*(cause) + rnorm(n, mean = mean(cause), sd = sd(cause))

  linear.MI[i]<-MutualInformation(cause, effect)
  linear.cor[i]<-cor(cause, effect)

  # sin cos
  ii<-runif(n)
  cause<-sin(2*pi*ii)
  effect<-cos(2*pi*ii)

  sin.MI[i]<-MutualInformation(cause, effect)
  sin.cor[i]<-cor(cause, effect)

  # log effect
  cause <- 1:n
  effect <- log(cause) + 3

  log.MI[i] <- MutualInformation(cause, effect)
  log.cor[i]<- cor(cause, effect)

  i<-i+1
}


x<-data.frame(function.method=rep("log.MI",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(log.MI))
#y<-data.frame(function.method=rep("log.Ker",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(log.KMI))
z<-data.frame(function.method=rep("log.cor",size),num.points=seq(from=100, to=size, by=100), MI=log.cor)

data.MI<-rbind(x,z)

p44<-ggplot(data=data.MI, aes(x=num.points, y=MI, group=function.method, colour=function.method)) +
  geom_line()+
  geom_point()

x<-data.frame(function.method=rep("sin.MI",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(sin.MI))
#y<-data.frame(function.method=rep("sin.Ker",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(sin.KMI))
z<-data.frame(function.method=rep("sin.cor",size),num.points=seq(from=100, to=size, by=100), MI=sin.cor)

data.MI<-rbind(x,z)

p22<-ggplot(data=data.MI, aes(x=num.points, y=MI, group=function.method, colour=function.method)) +
  geom_line() +
  geom_point()

x<-data.frame(function.method=rep("lin.MI",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(linear.MI))
#y<-data.frame(function.method=rep("lin.Ker",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(linear.KMI))
z<-data.frame(function.method=rep("lin.cor",size),num.points=seq(from=100, to=size, by=100), MI=linear.cor)

data.MI<-rbind(x,z)

p33<-ggplot(data=data.MI, aes(x=num.points, y=MI, group=function.method, colour=function.method)) +
  geom_line() +
  geom_point()


x<-data.frame(function.method=rep("rand.MI",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(random.MI))
#y<-data.frame(function.method=rep("rand.Ker",size),num.points=seq(from=100, to=size, by=100), MI=LambdaCorrelation(random.KMI))
z<-data.frame(function.method=rep("rand.cor",size),num.points=seq(from=100, to=size, by=100), MI=random.cor)

data.MI<-rbind(x,z)

p11<-ggplot(data=data.MI, aes(x=num.points, y=MI, group=function.method, colour=function.method)) +
  geom_line() +
  geom_point()




# random effect
set.seed(8192)
effect <- runif(n)
set.seed(192)
cause <- runif(n)

myplot<-data.frame(cause,effect)

p1<-ggplot(data=myplot, aes(x=cause, y=effect)) +
  geom_point()





ii<-runif(n)
cause<-sin(2*pi*ii)
effect<-cos(2*pi*ii)

myplot<-data.frame(cause,effect)

p2<-ggplot(data=myplot, aes(x=cause, y=effect)) +
  geom_point()




cause <- 1:n
effect <- 2*(cause) + rnorm(n, mean = mean(cause), sd = sd(cause))

myplot<-data.frame(cause,effect)

p3<-ggplot(data=myplot, aes(x=cause, y=effect)) +
  geom_point()



cause <- 1:n
effect <- log(cause) + 3

myplot<-data.frame(cause,effect)

p4<-ggplot(data=myplot, aes(x=cause, y=effect)) +
  geom_point()


multiplot(p1,p2,p3,p4,p11,p22,p33,p44,cols=2)











#####################
Entropy.Hist<-function(x){
  return(entropy(hist(x, plot=FALSE, breaks=20)$counts))
}




########## Load SPY returns
global.indices<-read.csv("data/global_indices_returns.csv")
SPY <- global.indices


############## ENTROPY
library(zoo)
## Calculate 252-rolling window entropy
SPY$SPY.entropy<-rollapply(SPY$X.GSPC, width = 252, by = 21, FUN = Entropy.Hist, align = "right")
## Plot
plot(SPY[!is.na(SPY$SPY.entropy),c("SPY.entropy")], main="SPY Daily Return 252-day Entropy")

