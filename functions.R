#Necessary functions
library(data.table)
library(quantmod)
library(qrmtools)

# Data loading and manipulation------------------------------------------------------------
#Function to download data from Yahoo Finance with the format from before
yahoo_load  <-  function(tickers, from, to=Sys.Date()){
  dtab  <-  data.table()
  for (i in seq_along(tickers)) {
    temp_dtab <- as.data.table(getSymbols(Symbols = tickers[i], src = "yahoo", 
                                        from = from, to = to, auto.assign = F))
    colnames(temp_dtab) <- c("Date", "Open", "High", "Low", "Close", "Volume", "AdjustedClose")
    temp_dtab[,Asset:=tickers[i]]
    dtab  <-  rbindlist(list(dtab, temp_dtab)) 
  }
  dtab
}


# Volatility stabilization ------------------------------------------------
vol_stabilization <- function(returns, convert_to_log=F, vol_window=20, lambda=0.03, 
                              tail_exp_window=3000, 
                              tail_exp_window_type=c("moving", "expanding")){
  if(convert_to_log){
    returns <- log(returns+1)
  }
  dtab <- as.data.table(returns)
  #N-time points rolling std. dev.
  dtab[, vol:=frollapply(returns, n = vol_window, FUN = function(x) sd(x))]
  #EWMA 2 day volatility with lambda of 0.03
  dtab[, vol_ewma:=sqrt((1-lambda)*shift(vol)^2+lambda*returns^2)]
  #Assume tail exponent of 2.4 - will later be expanded to MLE estimation of Pareto dist.
  tail_exp <- 2.4
  dtab[, vol_stable:=numeric(nrow(dtab))]
  dtab[vol_window+1, vol_stable:=vol]
  for(i in (vol_window+2):nrow(dtab)){
    vol_before <- dtab[i-1, vol_stable]
    dtab[i, vol_stable:=fifelse(abs(returns)<=tail_exp*vol_before,
                                sqrt((1-lambda)*vol_before^2+lambda*returns^2),
                                vol_before)]
  }
  dtab
}


