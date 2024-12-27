#Function to stabilize volatility according to Professor Douady's method
#Download data from yahoo
spx <- yahoo_load(tickers = "^GSPC", from = "1995-01-01")
spx[, return:=AdjustedClose/shift(AdjustedClose)-1]
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

vol_stabilization <- function(returns, convert_to_log=F, vol_window=20, tail_exp_window,
                              tail_exp_method=c("MLE", "MOM", "PWM")){
  if(convert_to_log){
    returns <- log(returns+1)
  }
  
}
