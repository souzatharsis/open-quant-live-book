#Datasets.R

GetIntrinio <- function(symbol, attribute, key, frequency){
  library(httr)
  http.string<-paste0("https://api-v2.intrinio.com/historical_data/",
                      symbol,
                      "/",
                      attribute,
                      "?api_key=",
                      key,
                      "&frequency=",
                      frequency)

  r<-GET(http.string)
  r.json<-content(r, "parsed")

  symbol.mcap<-data.frame(matrix(unlist(r.json), nrow=length(r.json$historical_data),
                                 byrow=T),stringsAsFactors=FALSE)
  names(symbol.mcap)<-c("Date", ticker)
  return(symbo.mcap)
}

GetIntrinio.MCap <- function(symbol, key, frequency="quarterly"){
  attribute<-"marketcap"
  return(GetIntrinio(symbol, attribute, key, frequency))
}


