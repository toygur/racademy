
# Fonksiyonlar - Yahoo finance data çekme #

getYahoo <- function(tickerList,
                     from="2017-01-01",
                     end="2018-01-01",
                     pricetype="AdjClose" ){
  
  library(quantmod)
  
  if (pricetype == "AdjClose") {
    type = 6
  } else if (pricetype == "Open") {
    type = 1
  } else if (pricetype == "High") {
    type = 2
  } else if (pricetype == "Low") {
    type = 3
  } else{
    type = 4
  } 

  data <- list()
  
  for(i in 1:length(tickerList)) {
    
    data[[i]] <- getSymbols(tickerList[i], 
                            src="yahoo", 
                            auto.assign = FALSE,
                            warnings = FALSE,
                            from = from, 
                            to = end)[,type]
  }
  
  data <- as.data.frame(data)
  colnames(data)<-tickerList
  
  return(data)
}

## TEST ##
tickerList <- c("^GSPC","MSFT","SBUX","XOM","AMZN")
pricetype="AdjClose"
from = "2015-01-01"
end = "2016-01-01"

data <- getYahoo(tickerList,from,end,pricetype)

