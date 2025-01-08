#Necessary functions
library(data.table)
library(RODBC)
# devtools::install_git(
#   "https://gitlab.smninvest.com/GK/smn-r-library.git",
#   quiet = FALSE,
#   force = TRUE
# )
library(smn)
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

