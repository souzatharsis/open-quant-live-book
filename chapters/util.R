############ OPTIONS
options(digits=3)

############ LIBRARIES
library(ggplot2)
library(config)
library(scales)
library(formatR)
############ Style

## Colours
colours<-brewer_pal("qual", "Set1")(6)


############ FUNCTIONS

set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path
  setwd(dirname(current_path ))
  print( getwd() )
}


# Function printf
printf <- function(...) invisible(cat(sprintf(...)))

stop0 <- function(...) stop(..., call.=FALSE);

getCurrentDate <- function() {
  date = Sys.Date();
  date = convertDate2(date);
}

convertYahooDate <- function(date) {
  date2 = format(strptime(date,"%Y-%m-%d"),"%Y%m%d");
  return(as.numeric(date2));
}


getCurrentHour <- function() {
  time  = Sys.time();
  time2 = format(strptime(time, "%Y-%m-%d %H:%M:%S"));
  time2 = gsub("^.* ", "", time2);
  time2 = gsub(":", "", time2);
  return(time2);
}

checkPackage <- function(package) {
  if (!package %in% rownames(installed.packages())) {
    install.packages(package);
  }
}

checkDateFormat <- function(date) {

  grepl("^[0-9]{8}$", as.character(date), perl=TRUE)
}

dateInPrintFormat <- function(date) {
  date2 = format(strptime(date, "%Y%m%d"),"%d/%m/%Y");
  return(date2);
}


loadMarketData <- function(cps, firstDate, lastDate) {


  prices = read.csv(paste0("../../data/IBOV.csv"), stringsAsFactors=FALSE);
  for (i in 1 : nrow(prices)) {
    prices[i,1] = convertYahooDate(prices[i,1]);
  }

  prices <- prices[!(prices[[2]] == prices[[3]] && prices[[2]] == prices[[4]] && prices[[2]] == prices[[5]]),];
  prices <- prices[,(-2:-6)];
  prices[,1] <- rev(prices[,1]);
  prices[,2] <- rev(prices[,2]);
  colnames(prices)[1] = "Date";
  colnames(prices)[2] = "IBOV";
  prices <- prices[prices[[1]] >= firstDate, ];
  prices <- prices[prices[[1]] <= lastDate, ];

  for (i in 1 : length(cps)) {
    #prices[,(i+2)] = rep(-1, nrow(prices));
    #colnames(prices)[i+2] = cps[i];
    data = read.csv(paste0("../../data/", cps[i], ".csv"), stringsAsFactors=FALSE);
    for (j in 1 : nrow(data)) {
      data[j,1] = convertYahooDate(data[j,1]);
    }
    data <- data[,(-2:-6)];
    data[,1] <- rev(data[,1]);
    data[,2] <- rev(data[,2]);
    colnames(data)[1] = "Date";

    prices <- merge(x = prices, y = data, by="Date", all.x = TRUE);
    colnames(prices)[i+2] = cps[i];
  }
  prices[is.na(prices)] <- -1;


  toRemove = numeric(0);
  for (i in 1 : nrow(prices)) {
    found = 0;
    for (j in 3 : (length(cps)+2)) {
      if (prices[i,j] > 0) {
        found = 1;
        break;
      }
    }
    if (!found) toRemove = c(toRemove, i);
  }
  if (length(toRemove) > 0) prices <- prices[-toRemove, ];

  rownames(prices) = prices[,1];
  prices <- prices[,2:ncol(prices)];

  return (prices);
}

# RSI is computed as a score from -50 to 50
computeRSI <- function(data, days = 14, emaRatio = 0.3333) {

  suppressMessages(library("TTR"));
  rsi = matrix(0, nrow=nrow(data), ncol=ncol(data));

  for (j in 1 : ncol(data)) {
    for (i in 1 : nrow(data)) {

      end = i;
      begin = i - days;
      if (begin < 1) next;

      ## CALCULATE RSI
      POS = diff(data[begin:end, j]);
      NEG = POS;
      POS[POS < 0] = 0;
      NEG[NEG > 0] = 0;
      NEG = NEG * -1;

      rs = 100;

      emapos = EMA(POS, n=days, ratio=emaRatio);
      emapos = emapos[length(emapos)];
      emaneg = EMA(NEG, n=days, ratio=emaRatio);
      emaneg = emaneg[length(emaneg)];

      if (emaneg > 0) rs = emapos/emaneg;
      rsi[i,j] = (100 - (100 / (1 + rs))) - 50;
    }
  }
  rownames(rsi) = rownames(data);
  return(rsi);
}

calculateReturns <- function(prices) {
  p = as.matrix(prices);
  returns = diff(p) / p[-nrow(p),];
  returns = replace(returns, is.nan(returns), 0);
  returns = replace(returns, returns <= -1, 0);
  returns = as.data.frame(returns);
  returns = rbind(rep(0, ncol(returns)), returns);
  rownames(returns) = rownames(prices);
  return(returns);
}

calculateLogReturns <- function(prices, dates=numeric(), muteTimePrint=FALSE, excludeFirstRow = FALSE) {
  p = as.matrix(prices);
  returns = diff(log(p));
  returns = replace(returns, is.nan(returns), 0);
  returns = replace(returns, returns <= -1, 0);
  returns = as.data.frame(returns);
  returns = rbind(rep(0, ncol(returns)), returns);
  rownames(returns) = rownames(prices);
  return(returns);
}
