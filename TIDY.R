library(tidyquant)
library(BatchGetSymbols)
library(quantmod)

NEWLIST <- xts()

df.SP500 <- GetSP500Stocks()
tickersfive <- df.SP500$tickers

tickersfive <- tickersfive[! tickersfive %in% c("CAT", "ATVI","ABBV","AOS","BRKB","BRK.B","BHF","BFB", "BF.B","SPG","BBY","BFB", "CGB","CHD","COP","DHI","ESS", "KIM","KSS", "MMC","CBG","NLSN","MTD","NLSON","UA")]

# cool progress bar to see the % of completion
n <- length(tickersfive)
pb <- txtProgressBar(min = 0, max = n, style=3)

startdatetick <- as.Date("2008-12-31")
enddatetick <- as.Date("2017-01-01")


for(i in 328:length(tickersfive)){
  
 symbol <- tickersfive[i]
  
  tryitnext <- try(tq_get(symbol, get = "key.ratios"))
  
  
  if(inherits(tryitnext, "try-error")){
    i <- i + 1
  }
  else {
    
    Check <- tq_get(symbol, get = "key.ratios")
    Checkint <- Check$data[[1]]
    D <- Checkint$date[1]
   y <- year(as.POSIXlt(D,format = "%Y/%m/%d"))
    
   if(y <= 2011){
     
     NEWLIST[[i]] = symbol
   }
   else{
   
   i <- i + 1
   
   }
  }
    
  setTxtProgressBar(pb, i) 
}

B <- NEWLIST[!is.na(NEWLIST)]
LISTFINAL <- coredata(B)

