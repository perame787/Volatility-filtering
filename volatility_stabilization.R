#Function to stabilize volatility according to Professor Douady's method
#Download data from yahoo
spx <- yahoo_load(tickers = "^GSPC", from = "1995-01-01")
spx[, return:=AdjustedClose/shift(AdjustedClose)-1]
spx[is.na(return), return:=0]
spx[, return_log:=c(0, exp(diff(log(AdjustedClose)))-1)]

#20 day rolling volatility
spx[, vol:=frollapply(return, n = 20, FUN = function(x) sd(x))]

#EWMA 2 day volatility with lambda of 0.03
lambda <- 0.03
spx[, vol_ewma:=sqrt((1-lambda)*shift(vol)^2+lambda*return^2)]

#Assume tail exponent of 2.4
tail_exp <- 2.4

#It's saddly recursive, do for loop
spx[, vol_stable:=numeric(nrow(spx))]
spx[21, vol_stable:=vol]

start<-Sys.time()
for(i in 22:nrow(spx)){
  vol_before <- spx[i-1, vol_stable]
  spx[i, vol_stable:=fifelse(abs(return)<=tail_exp*vol_before,
                             sqrt((1-lambda)*vol_before^2+lambda*return^2),
                             vol_before)]
}
end<-Sys.time()
end-start

plot(spx$Date, spx$vol_ewma, type="l")
lines(spx$Date, spx$vol_stable, type="l", col="red4")

#GPD estimation using absolute returns
spx[,abs_return:=abs(return)]
quantile(spx$abs_return, 0.9)
abs_rets <- spx$abs_return
excesses <- abs_rets[abs_rets>quantile(abs_rets, 0.9)]-quantile(abs_rets, 0.9)
fit_GPD_PWM(x = excesses)

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

vol_1<-vol_stabilization(returns = spx$return, tail_exp_window_type = "expanding")
plot(spx$Date, spx$vol, type="l")
lines(spx$Date, spx$vol_stable, type="l", col="red4")

plot(vol_1$vol, type="l")
lines(vol_1$vol_stable, col="red4")
