#Main "package"
library(PortfolioAnalytics)
library(portfolio.optimization)
library(qrmtools)
library(qrng)
library(quantmod)
library(rvinecopulib)
library(spd)
library(xtable)
library(parallel)
library(data.table)
library(zoo)
#### Loading and cleaning data ####
#prices_ss<-read.csv("C:/Users/Pedro/Documents/Studium/WU Dokummente/SoSe 23/Thesis/Scripts/Data/prices_ss.csv")
#ss_log<-fread("C:/Users/Pedro/Documents/Studium/WU Dokummente/SoSe 23/Thesis/Scripts/Data/scenario.set_log.csv")
#ss_simple<-fread("C:/Users/Pedro/Documents/Studium/WU Dokummente/SoSe 23/Thesis/Scripts/Data/scenario.set_simple.csv")

#Function to download data from Yahoo Finance and manipulate it to use as input in backtester
yahoo_load <- function(tickers, from, to){
  dtab <- data.table()
  for (i in seq_along(tickers)) {
    temp_dtab<-as.data.table(getSymbols(Symbols = tickers[i], src = "yahoo", 
                                        from = from, to = to, auto.assign = F))
    colnames(temp_dtab)<-c("Date", "Open", "High", "Low", "Close", "Volume", "AdjustedClose")
    temp_dtab[,Asset:=tickers[i]]
    dtab <- rbindlist(list(dtab, temp_dtab)) 
  }
  dtab
}

#Build data set with just adj. close prices of assets
prices.ts<-function(tickers, as.xts=TRUE){
  if(as.xts){
    prices<-na.omit(merge(get(tickers[1])[, 4], 
                          get(tickers[2])[, 4]))
    for(i in 3:(length(tickers))){
      prices<-na.omit(merge(prices, get(tickers[i])[, 4]))
    } 
  }
  else{
    prices<-na.omit(merge(get(tickers[1])[, c(1, 7)], 
                          get(tickers[2])[, c(1, 7)], 
                          by=c("index"), all=T))
    for(i in 3:(length(tickers))){
      prices<-na.omit(merge(prices, get(tickers[i])[, c(1,7)], by=c("index"), all=T))
    } 
  }
  colnames(prices)[1]<-"Date"
  prices
}

#Build empirical scenario set using log returns*(-1) of the prices_ss df
loss.dist<-function(price_df, names, ret_type=c("log", "simple"), 
                    neg_rets=T, no_zeroes=TRUE){
  price_df<-price_df[,-1]
  #Number of rows - 1 because of NAs when computing returns
  df<-matrix(NA, nrow = (nrow(price_df)-1), ncol = ncol(price_df))
  colnames(df)<-names
  #df[,1]<-price_df[-1,1]
  for(i in 1:ncol(df)){
    if(ret_type=="log"){
      df[,i]<-na.omit(diff(log(unlist(price_df[, i]))))
    }
    else if(ret_type=="simple"){
      df[,i]<-(exp(na.omit(diff(log(unlist(price_df[, i])))))-1)
    }
  }
  #df<-df[, -1]
  if(no_zeroes==TRUE){
    df<-df[apply(df!=0, 1, all),]
  }
  if(neg_rets==TRUE){
    df<-df*(-1)
  }
  df
}

#### Semiparametric return distribution (Carmona, 2014) ####
#Until now, only for right tail
#Parameter estimation
gpd.fit<-function(data, lower=NULL, upper=NULL, min.obs=150, 
                  method=c("MLE", "MOM", "PWM"), lower.tail=FALSE, double.tail){
  data<-as.matrix(data)
  #Fit for upper and lower tail
  if(double.tail==TRUE){
    if(is.null(lower)){
      lower<-ecdf(data)(head(sort(data), min.obs)[min.obs])
    }
    u.lower<-quantile(data, lower)
    excesses.lower<-data[data<=u.lower]*(-1)-u.lower*(-1)
    if(is.null(upper)){
      upper<-ecdf(data)(tail(sort(data), min.obs)[1])
    }
    u.upper<-quantile(data, upper)
    excesses.upper<-data[data>=u.upper]-u.upper
    #Safety check
    if(all(excesses.lower>=0) & all(excesses.upper>=0)==FALSE){
      stop("Support of the GPD is >=0. Check upper and lower thresholds.")
    }
    if(method=="MLE"){
      gpd.fit.lower<-fit_GPD_MLE(x = excesses.lower, estimate.cov = F)
      gpd.fit.upper<-fit_GPD_MLE(x = excesses.upper, estimate.cov = F)
    }
    else if(method=="MOM"){
      gpd.fit.lower<-fit_GPD_MOM(x = excesses.lower)
      gpd.fit.upper<-fit_GPD_MOM(x = excesses.upper)
    }
    else if(method=="PWM"){
      gpd.fit.lower<-fit_GPD_PWM(x = excesses.lower)
      gpd.fit.upper<-fit_GPD_PWM(x = excesses.upper)
    }
    list("Excesses.lower"=sort(excesses.lower), "Shape.lower"=gpd.fit.lower$par[["shape"]], 
         "Scale.lower"=gpd.fit.lower$par[["scale"]], "Threshold.lower"=u.lower,
         "Excesses.upper"=sort(excesses.upper), "Shape.upper"=gpd.fit.upper$par[["shape"]], 
         "Scale.upper"=gpd.fit.upper$par[["scale"]], "Threshold.upper"=u.upper)
  }
  #Fit for lower tail alone
  else{
    if(lower.tail==TRUE){
      #For no predefined threshold: select quantile such that n=min.obs
      if(is.null(lower)){
        lower<-ecdf(data)(head(sort(data), min.obs)[min.obs])
      }
      u<-quantile(data, lower)
      excesses<-data[data<=u]*(-1)-u*(-1)
    }
    #Fit for upper tail alone
    else{
      if(is.null(upper)){
        upper<-ecdf(data)(tail(sort(data), min.obs)[1])
      }
      u<-quantile(data, upper)
      excesses<-data[data>=u]-u 
    }
    #Safety check
    if(all(excesses>=0)==FALSE){
      stop("Support of the GPD is >=0")
    }
    if(method=="MLE"){
      gpd.fit<-fit_GPD_MLE(x = excesses, estimate.cov = F)
    }
    else if(method=="MOM"){
      gpd.fit<-fit_GPD_MOM(x = excesses)
    }
    else if(method=="PWM"){
      gpd.fit<-fit_GPD_PWM(x = excesses)
    }
    if(lower.tail==T){
      excesses<-excesses*(-1)
    }
    list("Excesses"=sort(excesses), "Shape"=gpd.fit$par[["shape"]], 
         "Scale"=gpd.fit$par[["scale"]], "Threshold"=u) 
  }
}

#With SE
gpd.fit.se<-function(data, lower=NULL, upper=NULL, min.obs=150, 
                 method=c("MLE", "MOM", "PWM"), lower.tail=FALSE, double.tail){
  data<-as.matrix(data)
  #Fit for upper and lower tail
  if(double.tail==TRUE){
    if(is.null(lower)){
      lower<-ecdf(data)(head(sort(data), min.obs)[min.obs])
    }
    u.lower<-quantile(data, lower)
    excesses.lower<-data[data<=u.lower]*(-1)-u.lower*(-1)
    if(is.null(upper)){
      upper<-ecdf(data)(tail(sort(data), min.obs)[1])
    }
    u.upper<-quantile(data, upper)
    excesses.upper<-data[data>=u.upper]-u.upper
    #Safety check
    if(all(excesses.lower>=0) & all(excesses.upper>=0)==FALSE){
      stop("Support of the GPD is >=0. Check upper and lower thresholds.")
    }
    if(method=="MLE"){
      gpd.fit.lower<-tryCatch({
        fit_GPD_MLE(x = excesses.lower, estimate.cov = T)
      }, error=function(e){
        fit_GPD_MLE(x = excesses.lower, estimate.cov = F)
      })
      gpd.fit.upper<-tryCatch({
        fit_GPD_MLE(x = excesses.upper, estimate.cov = T)
      }, error=function(e){
        fit_GPD_MLE(x = excesses.upper, estimate.cov = F)
      })
    }
    else if(method=="MOM"){
      gpd.fit.lower<-fit_GPD_MOM(x = excesses.lower)
      gpd.fit.upper<-fit_GPD_MOM(x = excesses.upper)
    }
    else if(method=="PWM"){
      gpd.fit.lower<-fit_GPD_PWM(x = excesses.lower)
      gpd.fit.upper<-fit_GPD_PWM(x = excesses.upper)
    }
    if(length(gpd.fit.lower$SE)==0){
      se.lower.shape<-NA
      se.lower.scale<-NA
    }
    else{
      se.lower.shape<-gpd.fit.lower$SE[["shape"]]
      se.lower.scale<-gpd.fit.lower$SE[["scale"]]
    }
    if(length(gpd.fit.upper$SE)==0){
      se.upper.shape<-NA
      se.upper.scale<-NA
    }
    else{
      se.upper.shape<-gpd.fit.upper$SE[["shape"]]
      se.upper.scale<-gpd.fit.upper$SE[["scale"]]
    }
    list("Threshold.lower"=u.lower, "Excesses.lower"=sort(excesses.lower), 
         "Shape.lower"=gpd.fit.lower$par[["shape"]], "se.shape.lower"=se.lower.shape,
         "Scale.lower"=gpd.fit.lower$par[["scale"]], "se.scale.lower"=se.lower.scale,
         "Threshold.upper"=u.upper, "Excesses.upper"=sort(excesses.upper),
         "Shape.upper"=gpd.fit.upper$par[["shape"]], "se.shape.upper"=se.upper.shape,
         "Scale.upper"=gpd.fit.upper$par[["scale"]], "se.scale.upper"=se.upper.scale)
  }
  #Fit for lower tail alone
  else{
    if(lower.tail==TRUE){
      #For no predefined threshold: select quantile such that n=min.obs
      if(is.null(lower)){
        lower<-ecdf(data)(head(sort(data), min.obs)[min.obs])
      }
      u<-quantile(data, lower)
      excesses<-data[data<=u]*(-1)-u*(-1)
    }
    #Fit for upper tail alone
    else{
      if(is.null(upper)){
        upper<-ecdf(data)(tail(sort(data), min.obs)[1])
      }
      u<-quantile(data, upper)
      excesses<-data[data>=u]-u 
    }
    #Safety check
    if(all(excesses>=0)==FALSE){
      stop("Support of the GPD is >=0")
    }
    if(method=="MLE"){
      gpd.fit<-tryCatch({
        fit_GPD_MLE(x = excesses, estimate.cov = T)
      }, error=function(e){
        fit_GPD_MLE(x = excesses, estimate.cov = F)
      })
    }
    else if(method=="MOM"){
      gpd.fit<-fit_GPD_MOM(x = excesses)
    }
    else if(method=="PWM"){
      gpd.fit<-fit_GPD_PWM(x = excesses)
    }
    if(lower.tail==T){
      excesses<-excesses*(-1)
    }
    if(length(gpd.fit$SE)==0){
      se.shape<-NA
      se.scale<-NA
    }
    else{
      se.shape<-gpd.fit$SE[["shape"]]
      se.scale<-gpd.fit$SE[["scale"]]
    }
    list("Threshold"=u, "Excesses"=sort(excesses), 
         "Shape"=gpd.fit$par[["shape"]], "se.shape"=se.shape,
         "Scale"=gpd.fit$par[["scale"]], "se.scale"=se.scale) 
  }
}

#Cumulative distribution
pSPGPD<-function(x, data, fit){
  #Automatically detect if the input fit is double or single tailed
  double.tail<-ifelse(length(fit)>4, TRUE, FALSE)
  emp.cdf<-sort(data)
  #CDF with right and left tail
  if(double.tail==TRUE){
    shape.lower<-fit$Shape.lower
    shape.upper<-fit$Shape.upper
    scale.lower<-fit$Scale.lower
    scale.upper<-fit$Scale.upper
    u.lower<-fit$Threshold.lower
    u.upper<-fit$Threshold.upper
    n_u.lower<-length(fit$Excesses.lower)
    n_u.upper<-length(fit$Excesses.upper)
    n<-length(emp.cdf)
    if(x<u.lower){
      prob<-(n_u.lower/n)*(1+shape.lower*(abs(x)-abs(u.lower))/scale.lower)^(-1/shape.lower)
    }
    else if(u.lower<=x & x<=u.upper){
      most.similar<-which.min(abs(emp.cdf-x))
      #Beware of rounding above since discrete distribution
      if(x<emp.cdf[most.similar]){
        i<-most.similar-1
      }
      else{
        i<-most.similar
      }
      prob<-(i-0.5)/n
    }
    else if(x>u.upper){
      prob<-1-(n_u.upper/n)*(1+shape.upper*(x-u.upper)/scale.upper)^(-1/shape.upper)
    }
  }
  else{
    #Sort data vector to have empirical CDF
    shape<-fit$Shape
    scale<-fit$Scale
    u<-fit$Threshold
    n_u<-length(fit$Excesses)
    n<-length(emp.cdf)
    if(x<=u){
      most.similar<-which.min(abs(emp.cdf-x))
      #Beware of rounding above since discrete distribution
      if(x<emp.cdf[most.similar]){
        i<-most.similar-1
      }
      else{
        i<-most.similar
      }
      prob<-(i-0.5)/n
    }
    else if(x>u){
      prob<-1-(n_u/n)*(1+shape*(x-u)/scale)^(-1/shape)
    }
  }
  unname(prob)
}

pSPGPD.fast<-function(x, data, fit){
  #Automatically detect if the input fit is double or single tailed
  double.tail<-ifelse(length(fit)>6, TRUE, FALSE)
  emp.cdf<-sort(data)
  #Assign "ID" to each observation to order them later
  x<-cbind(x, seq_along(x))
  #CDF with right and left tail
  if(double.tail==TRUE){
    shape.lower<-fit$Shape.lower
    shape.upper<-fit$Shape.upper
    scale.lower<-fit$Scale.lower
    scale.upper<-fit$Scale.upper
    u.lower<-fit$Threshold.lower
    u.upper<-fit$Threshold.upper
    n_u.lower<-length(fit$Excesses.lower)
    n_u.upper<-length(fit$Excesses.upper)
    n<-length(emp.cdf)
    #Observations below threshold
    x.lower<-x[x[,1]<u.lower,]
    x.lower[,1]<-(n_u.lower/n)*(1+shape.lower*(abs(x.lower[,1])-abs(u.lower))/scale.lower)^(-1/shape.lower)
    prob<-x.lower
    #prob<-(n_u.lower/n)*(1+shape.lower*(abs(x.lower)-abs(u.lower))/scale.lower)^(-1/shape.lower)
    #Observations between thresholds
    x.between<-x[u.lower<=x[,1] & x[,1]<=u.upper,]
    if(nrow(x.between)>0){
      most.similar<-sapply(x.between[,1], function(x) which.min(abs(emp.cdf-x)))
      i<-ifelse(x.between[,1]<emp.cdf[most.similar], most.similar-1, most.similar)
      x.between[,1]<-(i-0.5)/n
      prob<-rbind(prob, x.between)
      #prob<-c(prob, (i-0.5)/n)
    }
    #Observations above thresholds
    x.upper<-x[x[,1]>u.upper,]
    x.upper[,1]<-1-(n_u.upper/n)*(1+shape.upper*(x.upper[,1]-u.upper)/scale.upper)^(-1/shape.upper)
    prob<-rbind(prob, x.upper)
    #prob<-c(prob, 1-(n_u.upper/n)*(1+shape.upper*(x.upper-u.upper)/scale.upper)^(-1/shape.upper))
  }
  else{
    #Sort data vector to have empirical CDF
    shape<-fit$Shape
    scale<-fit$Scale
    u<-fit$Threshold
    n_u<-length(fit$Excesses)
    n<-length(emp.cdf)
    x.lower<-x[x[,1]<=u,]
    most.similar<-sapply(x.lower[,1], function(x) which.min(abs(emp.cdf-x)))
    i<-ifelse(x.lower[,1]<emp.cdf[most.similar], most.similar-1, most.similar)
    x.lower[,1]<-(i-0.5)/n
    prob<-x.lower
    #prob<-(i-0.5)/n
    x.upper<-x[x[,1]>u,]
    x.upper[,1]<-1-(n_u/n)*(1+shape*(x.upper[,1]-u)/scale)^(-1/shape)
    prob<-rbind(x.lower, x.upper)
    #prob<-c(prob, 1-(n_u/n)*(1+shape*(x.upper-u)/scale)^(-1/shape))
  }
  prob<-prob[order(prob[,2]),]
  #prob<-unname(prob)
  prob[,1]
}

#Quantile function
qSPGPD<-function(prob, data, fit){
  #Automatically detect if the input fit is double or single tailed
  double.tail<-ifelse(length(fit)>4, TRUE, FALSE)
  emp.cdf<-sort(data)
  if(double.tail==TRUE){
    shape.lower<-fit$Shape.lower
    shape.upper<-fit$Shape.upper
    scale.lower<-fit$Scale.lower
    scale.upper<-fit$Scale.upper
    u.lower<-fit$Threshold.lower
    u.upper<-fit$Threshold.upper
    #Find tho which quantiles do the thresholds belong
    prob_u.lower<-ecdf(emp.cdf)(u.lower)
    prob_u.upper<-ecdf(emp.cdf)(u.upper)
    n_u.lower<-length(fit$Excesses.lower)
    n_u.upper<-length(fit$Excesses.upper)
    n<-length(emp.cdf)
    if(prob<prob_u.lower){
      x<-((((prob*(n/n_u.lower))^(-shape.lower)-1)*scale.lower)/shape.lower+abs(u.lower))*(-1)
    }
    else if(prob_u.lower<=prob & prob<=prob_u.upper){
      #Security check: guarantee that it will not output a 0
      x<-ifelse(floor(prob*n+0.5)==0, emp.cdf[1], emp.cdf[floor(prob*n+0.5)])
    }
    else if(prob>prob_u.upper){
      x<-((((1-prob)*(n/n_u.upper))^(-shape.upper)-1)*scale.upper)/shape.upper+u.upper
    }
  }
  #Only right tail with GPD
  else{
    n<-length(emp.cdf)
    n_u<-length(fit$Excesses)
    shape<-fit$Shape
    scale<-fit$Scale
    u<-fit$Threshold
    #Find to which quantile does the threshold belong
    prob_u<-ecdf(emp.cdf)(u)
    if(prob<=prob_u){
      x<-ifelse(floor(prob*n+0.5)==0, emp.cdf[1], emp.cdf[floor(prob*n+0.5)])
    }
    else if(prob_u<prob){
      x<-((((1-prob)*(n/n_u))^(-shape)-1)*scale)/shape+u
    }
  }
  unname(x)
}

qSPGPD.fast<-function(prob, data, fit){
  #Automatically detect if the input fit is double or single tailed
  double.tail<-ifelse(length(fit)>6, TRUE, FALSE)
  emp.cdf<-sort(data)
  #Assign "ID" to each observation to order them later
  prob<-cbind(prob, seq_along(prob))
  if(double.tail==TRUE){
    shape.lower<-fit$Shape.lower
    shape.upper<-fit$Shape.upper
    scale.lower<-fit$Scale.lower
    scale.upper<-fit$Scale.upper
    u.lower<-fit$Threshold.lower
    u.upper<-fit$Threshold.upper
    #Find tho which quantiles do the thresholds belong
    prob_u.lower<-ecdf(emp.cdf)(u.lower)
    prob_u.upper<-ecdf(emp.cdf)(u.upper)
    n_u.lower<-length(fit$Excesses.lower)
    n_u.upper<-length(fit$Excesses.upper)
    n<-length(emp.cdf)
    #Probabilities below threshold
    prob.lower<-prob[prob[,1]<prob_u.lower,]
    prob.lower[,1]<-((((prob.lower[,1]*(n/n_u.lower))^(-shape.lower)-1)*scale.lower)/shape.lower+abs(u.lower))*(-1)
    #Probabilities between thresholds
    prob.between<-prob[prob_u.lower<=prob[,1] & prob[,1]<=prob_u.upper,]
    if(nrow(prob.between)>0){
      prob.between[,1]<-ifelse(floor(prob.between[,1]*n+0.5)==0, emp.cdf[1], emp.cdf[floor(prob.between[,1]*n+0.5)])
      x<-rbind(prob.lower, prob.between)
    }
    #Probabilities above thresholds
    prob.upper<-prob[prob[,1]>prob_u.upper,]
    prob.upper[,1]<-((((1-prob.upper[,1])*(n/n_u.upper))^(-shape.upper)-1)*scale.upper)/shape.upper+u.upper
    x<-rbind(x, prob.upper)
  }
  #Only right tail with GPD
  else{
    n<-length(emp.cdf)
    n_u<-length(fit$Excesses)
    shape<-fit$Shape
    scale<-fit$Scale
    u<-fit$Threshold
    prob_u<-ecdf(emp.cdf)(u)
    prob.lower<-prob[prob[,1]<=prob_u,]
    prob.lower[,1]<-ifelse(floor(prob.lower[,1]*n+0.5)==0, emp.cdf[1], emp.cdf[floor(prob.lower[,1]*n+0.5)])
    prob.upper<-prob[prob[,1]>prob_u,]
    prob.upper[,1]<-((((1-prob.upper[,1])*(n/n_u))^(-shape)-1)*scale)/shape+u
    x<-rbind(prob.lower, prob.upper)
  }
  x<-x[order(x[,2]),]
  x[,1]
}

#Random number generator
rSPGPD<-function(n, data, fit, quasirandom=TRUE){
  if(quasirandom==TRUE){
    q<-ghalton(n = n, d = 1, method = "halton")
  }
  else{
    q<-runif(n = n)
  }
  #Unlist for safety
  unlist(sapply(q, function(x) qSPGPD(prob = x, data = data, fit = fit)))
}

rSPGPD.fast<-function(n, data, fit, quasirandom=TRUE){
  if(quasirandom==TRUE){
    q<-ghalton(n = n, d = 1, method = "halton")
  }
  else{
    q<-runif(n = n)
  }
  #Unlist for safety
  qSPGPD.fast(prob = q, data = data, fit = fit)
}

#Plot of the CDF
pSPGPD.plot<-function(data, fit){
  emp.cdf<-sort(data)
  #Automatically detect if single or double tailed
  double.tail<-ifelse(length(fit)>4, TRUE, FALSE)
  if(double.tail==TRUE){
    u.lower<-fit$Threshold.lower
    u.upper<-fit$Threshold.upper
    x.lower<-emp.cdf[emp.cdf<u.lower]
    x.between<-emp.cdf[emp.cdf>=u.lower & emp.cdf<=u.upper]
    x.upper<-emp.cdf[emp.cdf>u.upper]
    y.lower<-sapply(x.lower, function(x) pSPGPD(x, data = data, fit = fit))
    y.between<-sapply(x.between, function(x) pSPGPD(x, data = data, fit = fit))
    y.upper<-sapply(x.upper, function(x) pSPGPD(x, data = data, fit = fit))
    plot(x.lower, y.lower, type="l", xlab="x", ylab="F(x)", 
         main="Semiparametric CDF - GPD tails",
         xlim=c(min(x.lower), max(x.upper)), ylim=c(0, 1), col="red2", cex=0.5, lwd=2)
    lines(x.between, y.between, type="l", col="blue2", lwd=2)
    lines(x.upper, y.upper, type="l", col="green2", lwd=2)
    abline(v=u.lower, col="grey20", lty=2)
    abline(v=u.upper, col="grey20", lty=2)
    legend("bottomright", legend = c(paste0("Lower=", names(u.lower)),
                                     paste0("Upper=", names(u.upper))),
           box.lty = 0)
  }
  else {
    u<-fit$Threshold
    x.lower<-emp.cdf[emp.cdf<=u]
    x.upper<-emp.cdf[emp.cdf>u]
    y.lower<-sapply(x.lower, function(x) pSPGPD(x, data = data, fit = fit))
    y.upper<-sapply(x.upper, function(x) pSPGPD(x, data = data, fit = fit))
    plot(x.lower, y.lower, type="l", xlab="x", ylab="F(x)", 
         main=paste0("Semiparametric CDF - GPD tail with u=", names(u)),
         xlim=c(min(x.lower), max(x.upper)), ylim=c(0, 1), col="blue2", cex=0.5, lwd=2)
    lines(x.upper, y.upper, type="l", col="green2", lwd=2)
    abline(v=u, col="grey20", lty=2)
  }
}

#### Scenario generation using EVT####
#Normal QQ Plots for all assets
qqplots<-function(df){
  par(mfrow=c(ceiling(ncol(df)/3), 3))
  tickers<-colnames(df)
  sapply(1:ncol(df), function(i){
    data<-unlist(df[, i])
    qqnorm(data, main = tickers[i])
    qqline(data)
  })
  par(mfrow=c(1,1))
}

#Mean excess plots for all assets
mean.excess.plot.grid<-function(df, gpd.fits=NULL, gpd.fits2=NULL, lower.excesses=FALSE){
  par(mfrow=c(ceiling(ncol(df)/3), 3))
  tickers<-colnames(df)
  sapply(1:ncol(df), function(i){
    #Take observations to the left of the distribution
    if(lower.excesses){
      data<-unlist(df[, i][df[,i]<0]*(-1))
      threshold<-gpd.fits[[i]]$Threshold.lower*(-1)
      if(!is.null(gpd.fits2)){
        threshold<-c(threshold, gpd.fits2[[i]]$Threshold.lower*(-1))
      }
    }
    else{
      data<-unlist(df[, i][df[,i]>0])
      threshold<-gpd.fits[[i]]$Threshold.upper
      if(!is.null(gpd.fits2)){
        threshold<-c(threshold, gpd.fits2[[i]]$Threshold.upper)
      }
    }
    mean_excess_plot(x = data, main = tickers[i], ylab=expression(e[n](u)), xlab="u")
    abline(v=threshold, lty=1:2)
  })
  par(mfrow=c(1,1))
}

#Shape plots for all assets
GPD.shape.plot.grid<-function(df, gpd.fits, gpd.fits2=NULL, lower.excesses=FALSE){
  par(mfrow=c(ceiling(ncol(df)/3), 3))
  tickers<-colnames(df)
  for(i in 1:ncol(df)){
    if(lower.excesses){
      data<-unlist(df[, i]*(-1))
      threshold<-gpd.fits[[i]]$Threshold.lower*(-1)
      if(!is.null(gpd.fits2)){
        threshold<-c(threshold, gpd.fits2[[i]]$Threshold.lower*(-1))
      }
    }
    else{
      data<-unlist(df[, i][df[,i]>0])
      threshold<-gpd.fits[[i]]$Threshold.upper
      if(!is.null(gpd.fits2)){
        threshold<-c(threshold, gpd.fits2[[i]]$Threshold.upper)
      }
    }
    tryCatch({
      GPD_shape_plot(x = data, estimate.cov = T, ylab = expression(xi), 
                     xlab = expression(paste("u and ", n[u])), xlab2 = tickers[i])
    }, error=function(e){
      GPD_shape_plot(x = data, estimate.cov = F, ylab = expression(xi), 
                     xlab = expression(paste("u and ", n[u])), xlab2 = tickers[i])
    })
    abline(v=threshold, lty=1:2)
  }
}

#Function to fit the semiparametric GPD to a scenario set using my function
spgpd.fit.list<-function(scenario.set, lower=NULL, upper=NULL, min.obs=150, 
                         method="MLE", lower.tail=FALSE, double.tail=FALSE){
  scenario.set<-as.matrix(scenario.set)
  tickers<-colnames(scenario.set)
  fits<-vector(mode="list", length(ncol(scenario.set)))
  fits<-lapply(1:length(tickers), function(i){
    loss_dist<-scenario.set[, tickers[i]]
    gpd.fit.se(data = loss_dist, lower, upper, min.obs, 
            method, lower.tail, double.tail)
  })
  names(fits)<-paste0(colnames(scenario.set), ".fit")
  fits
}

#Function to fit the semiparametric GPD to a scenario set using spd package
spd.fit.list<-function(scenario.set, upper, lower, tailfit, type, kernelfit){
  fits<-vector(mode="list", length(ncol(scenario.set)))
  fits<-lapply(1:ncol(scenario.set), function(i){
    loss_dist<-scenario.set[, i]
    spdfit(data = loss_dist, upper, lower, tailfit, type, kernelfit)
  })
  names(fits)<-paste0(colnames(scenario.set), ".fit")
  fits
}

#Table with the estimated parameters of a spgpd.fit.list object
fitted.param.tab<-function(gpd.fits, tickers){
  double.tail<-ifelse(length(gpd.fits)>4, TRUE, FALSE)
  if(double.tail){
    shape.lower<-lapply(gpd.fits, function(i) round(i$Shape.lower, 3))
    se.shape.lower<-lapply(gpd.fits, function(i) round(i$se.shape.lower, 3))
    scale.lower<-lapply(gpd.fits, function(i) round(i$Scale.lower, 3))
    se.scale.lower<-lapply(gpd.fits, function(i) round(i$se.scale.lower, 3))
    threshold.lower<-lapply(gpd.fits, function(i) round(i$Threshold.lower, 3))
    shape.upper<-lapply(gpd.fits, function(i) round(i$Shape.upper, 3))
    se.shape.upper<-lapply(gpd.fits, function(i) round(i$se.shape.upper, 3))
    scale.upper<-lapply(gpd.fits, function(i) round(i$Scale.upper, 3))
    se.scale.upper<-lapply(gpd.fits, function(i) round(i$se.scale.upper, 3))
    threshold.upper<-lapply(gpd.fits, function(i) round(i$Threshold.upper, 3))
    tab<-cbind(shape.lower, se.shape.lower, scale.lower, se.scale.lower,
               threshold.lower, shape.upper, se.shape.upper, scale.upper, 
               se.scale.upper, threshold.upper)
  }
  else{
    shape<-lapply(gpd.fits, function(i) round(i$Shape, 3))
    se.shape<-lapply(gpd.fits, function(i) round(i$se.shape, 3))
    scale<-lapply(gpd.fits, function(i) round(i$Scale, 3))
    se.scale<-lapply(gpd.fits, function(i) round(i$se.scale, 3))
    threshold<-lapply(gpd.fits, function(i) round(i$Threshold, 3))
    tab<-cbind(shape, se.shape, scale, se.scale, threshold)
  }
  rownames(tab)<-tickers
  as.data.frame(tab)
}

#Create empirical copula
copula.emp<-function(scenario.set, fits, method=c("SPGPD", "spd")){
  scenario.set<-as.matrix(scenario.set)
  copula<-matrix(nrow = nrow(scenario.set), ncol = ncol(scenario.set))
  names<-colnames(scenario.set)
  if(method=="SPGPD"){
    for(i in 1:ncol(scenario.set)){
      fit_i<-fits[[i]]
      ss_i<-scenario.set[,i]
      copula[,i]<-pSPGPD.fast(x = ss_i, data = ss_i, fit = fit_i)
    }
  }
  else if(method=="spd"){
    copula<-sapply(1:ncol(scenario.set), function(i){
      copula[,i]<-pspd(scenario.set[, i], fit = fits[[i]])
    }) 
  }
  colnames(copula)<-names
  copula
}

#copula_simple<-copula.emp(scenario.set = ss_simple, fitted_param = gpd_simple)
scen.generator<-function(copula_df=NULL, vinecop=NULL, scenario.set=NULL, fits, 
                             method=c("SPGPD", "spd"), qrng=TRUE, 
                             t=nrow(scenario.set)){
  inverse_df<-matrix(nrow = nrow(scenario.set), ncol = ncol(scenario.set))
  colnames(inverse_df)<-colnames(scenario.set)
  #Non-parametric case where empirical copula is used
  if(is.null(vinecop)){
    #Bootstrap
    copula_df<-copula_df[sample(x = 1:nrow(copula_df), size = t, replace = T),] 
  }
  #Case where an object of class vinecop is input
  else{
    copula_df<-matrix(data = NA, nrow = t, ncol = ncol(scenario.set))
    copula_df<-rvinecop(n = t, vinecop = vinecop, qrng = qrng, cores=12)
  }
  #Apply inverse transform for each column of the bootstraped copula
  if(method=="SPGPD"){
    for(i in 1:ncol(copula_df)){
      fit_i<-fits[[i]]
      q_i<-copula_df[,i]
      ss_i<-scenario.set[,i]
      inverse_df[,i]<-qSPGPD.fast(prob = q_i, data = ss_i, fit = fit_i)
    }
  }
  else if(method=="spd"){
    inverse_df<-sapply(1:ncol(copula_df), function(i){
      inverse_df[,i]<-qspd(copula_df[, i], fit = fits[[i]])
    })
  }
  else if(method=="normal"){
    inverse_df<-sapply(1:ncol(copula_df), function(i){
      inverse_df<-qnorm(copula_df[,i], mean = mean(scenario.set[,i]), sd = sd(scenario.set[,i]))
    })
  }
  inverse_df
}

#Generate list of scenario sets
scen.generator.list<-function(copula_df=NULL, vinecop=NULL, scenario.set=NULL, fits, 
                              method="SPGPD", qrng=TRUE, n=1000, 
                              t=nrow(scenario.set)){
  names<-colnames(scenario.set)
  ncol_ss<-length(names)
  #Generate list with empty matrices
  scenario_set_list<-lapply(1:n, matrix, data=NA, nrow=t, ncol=ncol_ss)
  if(is.null(vinecop)){
    scenario_set_list<-lapply(scenario_set_list, function(i){ 
      scen.generator(copula_df = copula_df, scenario.set = scenario.set, fits = fits, 
                     method = method, qrng = qrng, t = t)
    })
  }
  else{
    scenario_set_list<-lapply(scenario_set_list, function(i){ 
      scen.generator(vinecop = vinecop, scenario.set = scenario.set, fits = fits, 
                     method = method, qrng = qrng, t = t)
    })
  }
  scenario_set_list
}

#### Portfolio optimization ####
#Functions to update portfolio.optimization package
min.mean <- function(model, value) {  
  model$fix.mean <- NULL
  model$min.mean <- value
  return(model)
}

optimal.portfolio.markowitz <- function(model) {
  
  ### Variables: x[asset]
  
  n_var <- model$assets
  ix_x <- 1
  
  ### Objective function
  ### minimize { t(x) * Cov(data) * x }
  
  Objective <- list()
  Objective$quadratic <- cov(model$data)
  Objective$linear <- rep(0, model$assets)
  
  ### Constraints
  
  Constraints <- list(n=n_var, A=NULL, b=NULL, Aeq=NULL, beq=NULL)
  
  # sum(a) { x[a] } == sum.portfolio
  Constraints <- linear.constraint.eq(Constraints, c(1:model$assets), model$sum.portfolio)
  
  # sum(a) { x[a] * mean[a] } => min.mean
  sign <- -1
  #if (model$min.mean > mean(colMeans(model$data))) { sign <- -1 } # !!! only if scenario.data available
  if(!is.null(model$min.mean)) { Constraints <- linear.constraint.iq(Constraints, c((ix_x):(ix_x+model$assets-1)), sign * model$min.mean, sign * model$asset.means) }
  
  # sum(a) { x[a] * mean[a] } == fix.mean
  if(!is.null(model$fix.mean)) { Constraints <- linear.constraint.eq(Constraints, c((ix_x):(ix_x+model$assets-1)), model$fix.mean, model$asset.means) }
  
  ### Bounds
  Bounds <- list()
  Bounds$lower <- model$asset.bound.lower
  Bounds$upper <- model$asset.bound.upper
  
  ### Solve optimization problem using modopt.quadprog
  solution <- modopt.matlab::quadprog(Objective$quadratic, Objective$linear, Constraints$A, Constraints$b, Constraints$Aeq, Constraints$beq, Bounds$lower, Bounds$upper)
  
  ### Add optimal portfolio to model  
  portfolio <- list()
  portfolio$x <- solution$x
  portfolio$x <- round(portfolio$x, model$precision)  
  model$portfolio <- portfolio
  return(model) 
}

#Function to compute Expected Shortfall
ES.discrete<-function(loss, alpha, tail=c("right", "left")){
  if(tail=="right"){
    VaR<-quantile(loss, probs = alpha)
    exc<-which(loss>=VaR)
  }
  else if(tail=="left"){
    VaR<-quantile(loss, probs = alpha)
    exc<-which(loss<=VaR)
  }
  es<-mean(loss[exc])
  es
}

#Portfolio optimization under ES
stoch.optim<-function(scenario.set, method=c("ES", "var"), conf_level=NULL, 
                      min_weight=NULL, max_weight=NULL, pos_limit=NULL, mean=NULL){
  names<-colnames(scenario.set)
  if(method=="ES"){
    if(sum(class(scenario.set)=="xts")==0){
      scenario.set<-xts(scenario.set, order.by = Sys.Date()+1:nrow(scenario.set))
    }
    init.portf<-portfolio.spec(assets = ncol(scenario.set))
    init.portf<-add.constraint(portfolio=init.portf, type="full_investment")
    init.portf<-add.constraint(portfolio=init.portf, type="long_only")
    # Add position limit constraint such that we have a maximum number
    # of three assets with non-zero weights.
    if(!is.null(min_weight)){
      init.portf<-add.constraint(portfolio = init.portf, type = "box", 
                                 min=min_weight, max=max_weight)
    }
    if(!is.null(pos_limit)){
      init.portf<-add.constraint(portfolio = init.portf, type="position_limit", max_pos=pos_limit)
    }
    if(!is.null(mean)){
      init.portf<-add.constraint(portfolio=init.portf, type="return", 
                                 return_target=mean)
    }
    init.portf<-add.objective(portfolio=init.portf, type="risk", name="ES",
                                arguments=list(p=(1-conf_level)))
    opt<-optimize.portfolio(R=scenario.set, portfolio=init.portf, 
                                optimize_method="ROI", 
                                trace=TRUE)
    opt_weights<-opt$weights
  }
  else if(method=="var"){
    model<-portfolio.model(input = as.matrix(scenario.set))
    if(!is.null(mean)){
      model<-min.mean(model, mean)
    }
    opt<-optimal.portfolio.markowitz(model)
    opt_weights<-unlist(opt$portfolio)
  }
  names(opt_weights)<-names
  opt_weights
}

#es_optim(scenario.set = ss_log, method = "ES", conf_level = 0.01, mean = 0)
#es_optim(scenario.set = ss_log_boot, method = "ES", conf_level = 0.01, mean = 0)
#debug(es_optim)

#Efficient portfolio using Monte Carlo
stoch.optim.mc<-function(scenario_set_list, method=c("ES", "var"), 
                           conf_level=NULL, min_weight=NULL, max_weight=NULL, 
                           pos_limit=NULL, mean=NULL){
  names<-colnames(scenario.set)
  opt.weight.matrix<-matrix(data = NA, nrow = length(scenario_set_list), ncol = length(names))
  colnames(opt.weight.matrix)<-names
  cl<-makeCluster(detectCores())
  clusterEvalQ(cl, source("C:/Users/Pedro/Documents/Studium/WU Dokummente/SoSe 23/Thesis/Scripts/package.R"))
  clusterExport(cl, c("method", "conf_level", "min_weight", "max_weight", "pos_limit"), envir = environment())
  opt.weight.matrix<-t(parSapply(cl, scenario_set_list, function(i){
    stoch.optim(scenario.set = i*(-1), method = method, conf_level = conf_level, 
                min_weight = min_weight, max_weight = max_weight, 
                pos_limit = pos_limit, mean = mean)
  }))
  stopCluster(cl)
  weights_mc<-colMeans(opt.weight.matrix)
  std_errors<-apply(opt.weight.matrix, MARGIN = 2, function(i) sd(i)/sqrt(length(i)))
  rbind("Weight"=weights_mc, "Std. error"=std_errors)
}

#Compute efficient frontier 
eff.frontier<-function(scenario.set, mu_sigma_space=c("ES", "var"), conf_level=NULL){
  scenario.set<-as.matrix(scenario.set)
  asset_mean<-colMeans(scenario.set)
  frontier_size<-ncol(scenario.set)+5
  frontier <- seq(min(asset_mean), max(asset_mean), length.out=frontier_size)
  frontier_mean <- c()
  frontier_sd <- c()
  frontier_var<-c()
  frontier_es90<-c()
  frontier_es95<-c()
  frontier_es99<-c()
  frontier_es_ci<-c()
  for(i in 2:(frontier_size-1)){
    if(mu_sigma_space=="ES"){
      portfolio<-stoch.optim(scenario.set = scenario.set, method = "ES", 
                             conf_level = conf_level, mean = frontier[i])
    }
    else if(mu_sigma_space=="var"){
      portfolio<-stoch.optim(scenario.set = scenario.set, method = "var", 
                             conf_level = conf_level, mean = frontier[i])
    }
    loss<-scenario.set %*% portfolio
    frontier_mean<-c(frontier_mean, mean(loss))
    frontier_sd<-c(frontier_sd, sd(loss))
    frontier_var<-c(frontier_var, quantile(loss*(-1), probs = (1-conf_level)))
    frontier_es90<-c(frontier_es90, ES.discrete(loss = loss*(-1), 
                                            alpha = 0.9, tail = "right"))
    frontier_es95<-c(frontier_es95, ES.discrete(loss = loss*(-1), 
                                            alpha = 0.95, tail = "right"))
    frontier_es99<-c(frontier_es99, ES.discrete(loss = loss*(-1), 
                                            alpha = 0.99, tail = "right"))
    frontier_es_ci<-c(frontier_es_ci, ES.discrete(loss = loss*(-1), 
                                            alpha = (1-conf_level), tail = "right"))
  }
  df<-as.data.frame(cbind(frontier_mean, frontier_sd, frontier_var, frontier_es90,
                          frontier_es95, frontier_es99, frontier_es_ci))
  df<-head(df, nrow(df)-1)
  rownames(df)<-1:nrow(df)
  df
}

#Efficient frontier function to be called doing MC simulations
eff.frontier.b<-function(scenario.set, mu_sigma_space=c("ES", "var"), conf_level=NULL){
  scenario.set<-as.matrix(scenario.set)
  asset_mean<-colMeans(scenario.set)
  frontier_size<-ncol(scenario.set)+5
  frontier <- seq(min(asset_mean), max(asset_mean), length.out=frontier_size)
  frontier_seq<-2:(frontier_size-1)
  frontier_seq_l<-length(frontier_seq)
  frontier_mean <- numeric(frontier_seq_l)
  frontier_sd <- numeric(frontier_seq_l)
  frontier_es95<-numeric(frontier_seq_l)
  frontier_es99<-numeric(frontier_seq_l)
  #loss<-numeric(nrow(scenario.set))
  losses <- matrix(NA, ncol = frontier_seq_l, nrow = nrow(scenario.set))
  portfolio_matrix<-sapply(frontier_seq, function(i){
    stoch.optim(scenario.set = scenario.set, method = mu_sigma_space, 
                conf_level = conf_level, mean = frontier[i])
  })
  losses<-scenario.set %*% portfolio_matrix
  frontier_mean<-colMeans(losses)
  frontier_sd<-apply(losses, 2, sd)
  frontier_es95<-apply(losses*(-1), 2, ES.discrete, alpha = 0.95, tail = "right")
  frontier_es99<-apply(losses*(-1), 2, ES.discrete, alpha = 0.99, tail = "right")
  df<-as.data.frame(cbind(frontier_mean, frontier_sd, frontier_es95, frontier_es99))
  df<-head(df, nrow(df)-1)
  rownames(df)<-1:nrow(df)
  df
}

#Function for MC estimate plus columns with lower and upper values for the CI
mc_ci<-function(param_matrix, conf_level_mc, param_name){
  mc_matrix<-matrix(data = NA, nrow = nrow(param_matrix), ncol = 3)
  mc_matrix[,2]<-rowMeans(param_matrix)
  sd<-apply(param_matrix, MARGIN = 1, FUN = sd)
  bounds<-(qnorm((1-(conf_level_mc/2)))*sd)/sqrt(nrow(param_matrix))
  mc_matrix[,1]<-mc_matrix[,2]-bounds
  mc_matrix[,3]<-mc_matrix[,2]+bounds
  colnames(mc_matrix)<-c(paste0(param_name, "_lb"), paste0(param_name, "_mc"), 
                         paste0(param_name, "_ub"))
  mc_matrix
}

#Efficient frontier with CI
eff.frontier.mc<-function(scenario_set_list, mu_sigma_space, conf_level, conf_level_mc=0.05){
  n<-length(scenario_set_list)
  frontier_size<-ncol(scenario_set_list[[1]])+5
  mu_matrix<-matrix(data = NA, nrow = frontier_size, ncol = n)
  sd_matrix<-matrix(data = NA, nrow = frontier_size, ncol = n)
  es95_matrix<-matrix(data = NA, nrow = frontier_size, ncol = n)
  es99_matrix<-matrix(data = NA, nrow = frontier_size, ncol = n)
  eff_frontier_list<-lapply(1:n, matrix, data=NA, nrow=frontier_size, ncol=4)
  eff_frontier_list<-lapply(scenario_set_list, function(i){
    eff.frontier.b(scenario.set = i*(-1), mu_sigma_space = mu_sigma_space, conf_level = conf_level)
  })
  mu_matrix<-sapply(eff_frontier_list, function(i) i[,1])
  sd_matrix<-sapply(eff_frontier_list, function(i) i[,2])
  es95_matrix<-sapply(eff_frontier_list, function(i) i[,3])
  es99_matrix<-sapply(eff_frontier_list, function(i) i[,4])
  mu_mc<-mc_ci(param_matrix = mu_matrix, conf_level_mc = conf_level_mc, param_name = "mu")
  sd_mc<-mc_ci(param_matrix = sd_matrix, conf_level_mc = conf_level_mc, param_name = "sd")
  es95_mc<-mc_ci(param_matrix = es95_matrix, conf_level_mc = conf_level_mc, param_name = "ES95")
  es99_mc<-mc_ci(param_matrix = es99_matrix, conf_level_mc = conf_level_mc, param_name = "ES99")
  as.data.frame(do.call(cbind,(list(mu_mc, sd_mc, es95_mc, es99_mc))))
}

eff.frontier.mc.par<-function(scenario_set_list, mu_sigma_space, conf_level, conf_level_mc=0.05){
  n<-length(scenario_set_list)
  frontier_size<-ncol(scenario_set_list[[1]])+5
  mu_matrix<-matrix(data = NA, nrow = frontier_size, ncol = n)
  sd_matrix<-matrix(data = NA, nrow = frontier_size, ncol = n)
  es95_matrix<-matrix(data = NA, nrow = frontier_size, ncol = n)
  es99_matrix<-matrix(data = NA, nrow = frontier_size, ncol = n)
  eff_frontier_list<-lapply(1:n, matrix, data=NA, nrow=frontier_size, ncol=4)
  #Parallelize
  cl<-makeCluster(detectCores())
  clusterEvalQ(cl, source("C:/Users/Pedro/Documents/Studium/WU Dokummente/SoSe 23/Thesis/Scripts/package.R"))
  eff_frontier_list<-parLapply(cl = cl, X = scenario_set_list, function(i){
    eff.frontier.b(scenario.set = i*(-1), mu_sigma_space = mu_sigma_space, conf_level = conf_level)
  })
  stopCluster(cl)
  mu_matrix<-sapply(eff_frontier_list, function(i) i[,1])
  sd_matrix<-sapply(eff_frontier_list, function(i) i[,2])
  es95_matrix<-sapply(eff_frontier_list, function(i) i[,3])
  es99_matrix<-sapply(eff_frontier_list, function(i) i[,4])
  mu_mc<-mc_ci(param_matrix = mu_matrix, conf_level_mc = conf_level_mc, param_name = "mu")
  sd_mc<-mc_ci(param_matrix = mu_matrix, conf_level_mc = conf_level_mc, param_name = "sd")
  es95_mc<-mc_ci(param_matrix = es95_matrix, conf_level_mc = conf_level_mc, param_name = "ES95")
  es99_mc<-mc_ci(param_matrix = es99_matrix, conf_level_mc = conf_level_mc, param_name = "ES99")
  as.data.frame(do.call(cbind,(list(mu_mc, sd_mc, es95_mc, es99_mc))))
}

#Table with optimal portfolios for different set-ups
opt_port_tab<-function(scenario.set.list, method=c("ES", "var"), conf_level=NULL, neg_rets=TRUE){
  optimals_tab<-as.data.frame(matrix(NA, nrow = length(scenario.set.list), 
                                     ncol = ncol(scenario.set.list[[1]])+2))
  if(neg_rets){
    scenario.set.list<-lapply(scenario.set.list, function(i) i*(-1))
  }
  for(i in 1:nrow(optimals_tab)){
    optimals_tab[i, 3:ncol(optimals_tab)]<-stoch.optim(scenario.set = scenario.set.list[[i]], 
                                                       method = method, conf_level = conf_level)
  }
  colnames(optimals_tab)<-c("Scenario set", "Method", colnames(scenario.set.list[[1]]))
  optimals_tab[,1]<-names(scenario.set.list)
  optimals_tab[,2]<-rep(method, nrow(optimals_tab))
  optimals_tab
}

#Risk report for optimal portfolios
risk_report<-function(opt_weights, scenario.set){
  loss<-scenario.set %*% as.numeric(opt_weights)
  time<-length(loss)/260
  cagr<-(exp(sum(loss))^(1/time)-1)*100
  ann_sd<-(sd(exp(loss)-1)*sqrt(260))*100
  var95<-quantile(loss*(-1), 0.95)*100
  es95<-ES.discrete(loss = loss*(-1), alpha = 0.95, tail = "right")*100
  es99<-ES.discrete(loss = loss*(-1), alpha = 0.99, tail = "right")*100
  c("CAGR"=cagr, "Ann. SD"=ann_sd, "VaR 95%"=var95, "ES 95%"=es95, "ES 99%"=es99)
}

#### Table of summary statistics for the scenario set ####
#CAGR
cagr<-function(scenario.set, neg_rets=FALSE){
  if(neg_rets){
    scenario.set<-scenario.set*(-1)
  }
  time<-nrow(scenario.set)/260
  cagr<-(exp(colSums(scenario.set))^(1/time)-1)*100
  cagr
}

#Annualized Std. Dev.
ann_sd<-function(scenario.set, log_rets=TRUE){
  if(log_rets){
    scenario.set<-exp(scenario.set)-1
  }
  std_dev<-apply(scenario.set, MARGIN = 2, sd)
  ann_sd<-std_dev*sqrt(260)
  ann_sd*100
}

#Skewness
skew<-function(scenario.set, neg_rets=FALSE){
  if(neg_rets){
    scenario.set<-scenario.set*(-1)
  }
  skew<-apply(scenario.set, MARGIN = 2, FUN = skewness)
  skew
}

#Kurtosis
kurt<-function(scenario.set){
  kurt<-apply(scenario.set, MARGIN = 2, FUN = kurtosis)
  kurt
}

#VaR
VaR_ss<-function(scenario.set, alpha, neg_rets=FALSE, log_rets=TRUE){
  if(neg_rets==FALSE){
    scenario.set<-scenario.set*(-1)
  }
  var<-numeric(ncol(scenario.set))
  var<-sapply(1:ncol(scenario.set), function(x) 
    quantile(scenario.set[,x], alpha))
  if(log_rets){
    unname((exp(var)-1)*100)
  }
  else{
    unname(var*100 )
  }
}

#ES
ES_ss<-function(scenario.set, alpha, neg_rets=FALSE, log_rets=TRUE){
  if(neg_rets==FALSE){
    scenario.set<-scenario.set*(-1)
  }
  ES<-numeric(ncol(scenario.set))
  for(i in 1:ncol(scenario.set)){
    ES[i]<-ES.discrete(loss = scenario.set[,i], alpha = alpha, tail = "right")
  }
  if(log_rets){
    unname((exp(ES)-1)*100)
  }
  else{
    unname(ES*100)
  }
}

#Shapiro-Wilk normality test
swilk_test<-function(scenario.set){
  swilk_stat<-apply(scenario.set, MARGIN = 2, FUN = function(i) shapiro.test(x = i)$statistic)
  swilk_pval<-apply(scenario.set, MARGIN = 2, FUN = function(i) shapiro.test(x = i)$p.val)
  swilk_sign<-swilk_pval<0.01
  swilk<-cbind(swilk_stat, swilk_pval, swilk_sign)
  swilk
}

#Summary table
summary_stats<-function(scenario.set, neg_rets, log_rets, alpha){
  scenario.set<-as.matrix(scenario.set)
  sum_tab<-as.data.frame(matrix(data = NA, nrow = ncol(scenario.set), ncol = 8))
  colnames(sum_tab)<-c("Asset", "CAGR (in %)", "Ann. SD (in %)", "Skewness",
                       "Kurtosis", paste(alpha, "VaR (in %)"), paste(alpha, "ES in (%)"),
                       "Shapiro-Wilk")
  sum_tab[,1]<-colnames(scenario.set)
  sum_tab[,2]<-round(cagr(scenario.set, neg_rets), 2)
  sum_tab[,3]<-round(ann_sd(scenario.set, log_rets), 2)
  sum_tab[,4]<-round(skew(scenario.set, neg_rets), 2)
  sum_tab[,5]<-round(kurt(scenario.set), 2)
  sum_tab[,6]<-round(VaR_ss(scenario.set, alpha, neg_rets, log_rets), 2)
  sum_tab[,7]<-round(ES_ss(scenario.set, alpha, neg_rets, log_rets), 2)
  sum_tab[,8]<-round(swilk_test(scenario.set)[,1], 2)
  sum_tab
}

#Get lower triangular matrix
get_triangular<-function(cor_matrix, which.tri=c("upper", "lower")){
  if(which.tri=="upper"){
    cor_matrix[lower.tri(cor_matrix)]<-NA
  }
  else if(which.tri=="lower"){
    cor_matrix[upper.tri(cor_matrix)]<-NA
  }
  cor_matrix
}

#Function to output table of different pair copula families in a vine
pair_cop_families<-function(vinecop){
  pair_copulas<-vinecop$pair_copulas
  family<-c()
  #Iterate over each tree
  for(j in 1:length(pair_copulas)){
    tree<-pair_copulas[[j]]
    for(i in 1:length(tree)){
      edge<-tree[[i]]
      family<-c(family, unlist(edge)[1])
    }
  }
  count<-as.matrix(table(family))
  count
}

# Plotting of results -----------------------------------------------------
#Efficiency frontiers
plot_eff_frontier<-function(eff_frontier_list, hist_ef_included=T, risk_measure, 
                            col_vector, title=NULL, ci_background=T){
  if(hist_ef_included){
    if(risk_measure=="ES95"){
      min_x<-min(min(eff_frontier_list[[1]]$frontier_es95), 
                 sapply(eff_frontier_list[2:3], function(i) min(i$ES95_mc)))
      max_x<-max(max(eff_frontier_list[[1]]$frontier_es95), 
                 sapply(eff_frontier_list[2:3], function(i) max(i$ES95_mc)))
      max_y<-max(sapply(eff_frontier_list[2:3], function(i) max(i$mu_ub)))
      plot(eff_frontier_list[[1]]$frontier_es95, eff_frontier_list[[1]]$frontier_mean, 
           type="l", col=col_vector[1], xlim=c(min_x-0.0005, max_x+0.0001), ylim=c(0, max_y+0.00001), 
           xlab=risk_measure, ylab=expression(paste(mu, "-MC")), main=title)
      if(ci_background){
        for(i in 2:length(eff_frontier_list)){
          polygon(c(eff_frontier_list[[i]]$ES95_mc, rev(eff_frontier_list[[i]]$ES95_mc)), 
                  c(eff_frontier_list[[i]]$mu_lb, rev(eff_frontier_list[[i]]$mu_ub)), 
                  col="grey90", border=F)
        }
        for(i in 2:length(eff_frontier_list)){
          lines(eff_frontier_list[[i]]$ES95_mc, eff_frontier_list[[i]]$mu_mc, col=paste0(col_vector[i], 3))
          lines(eff_frontier_list[[i]]$ES95_mc, eff_frontier_list[[i]]$mu_lb, col=col_vector[i], lty=2)
          lines(eff_frontier_list[[i]]$ES95_mc, eff_frontier_list[[i]]$mu_ub, col=col_vector[i], lty=2)
        }
      }
      lines(eff_frontier_list[[1]]$frontier_es95, eff_frontier_list[[1]]$frontier_mean, 
            type="l", col=col_vector[1])
      for(i in 2:length(eff_frontier_list)){
        lines(eff_frontier_list[[i]]$ES95_mc, eff_frontier_list[[i]]$mu_mc, col=paste0(col_vector[i], 3))
      }
    }
    else if(risk_measure=="ES99"){
      min_x<-min(min(eff_frontier_list[[1]]$frontier_es99), 
                 sapply(eff_frontier_list[2:3], function(i) min(i$ES99_mc)))
      max_x<-max(max(eff_frontier_list[[1]]$frontier_es95), 
                 sapply(eff_frontier_list[2:3], function(i) max(i$ES99_mc)))
      max_y<-max(sapply(eff_frontier_list[2:3], function(i) max(i$mu_ub)))
      plot(eff_frontier_list[[1]]$frontier_es99, eff_frontier_list[[1]]$frontier_mean, 
           type="l", col=col_vector[1], xlim=c(min_x-0.0005, max_x+0.0001), ylim=c(0, max_y+0.00001), 
           xlab=risk_measure, ylab=expression(paste(mu, "-MC")), main=title)
      if(ci_background){
        for(i in 2:length(eff_frontier_list)){
          polygon(c(eff_frontier_list[[i]]$ES99_mc, rev(eff_frontier_list[[i]]$ES99_mc)), 
                  c(eff_frontier_list[[i]]$mu_lb, rev(eff_frontier_list[[i]]$mu_ub)), 
                  col="grey90", border=F)
        }
      }
      lines(eff_frontier_list[[1]]$frontier_es99, eff_frontier_list[[1]]$frontier_mean, 
            type="l", col=col_vector[1])
      for(i in 2:length(eff_frontier_list)){
        lines(eff_frontier_list[[i]]$ES99_mc, eff_frontier_list[[i]]$mu_mc, col=paste0(col_vector[i], 3))
        lines(eff_frontier_list[[i]]$ES99_mc, eff_frontier_list[[i]]$mu_lb, col=col_vector[i], lty=2)
        lines(eff_frontier_list[[i]]$ES99_mc, eff_frontier_list[[i]]$mu_ub, col=col_vector[i], lty=2)
      }
    }
    else if(risk_measure=="var"){
      min_x<-min(min(eff_frontier_list[[1]]$frontier_sd), 
                 sapply(eff_frontier_list[2:3], function(i) min(i$sd_mc)))
      max_x<-max(max(eff_frontier_list[[1]]$frontier_sd), 
                 sapply(eff_frontier_list[2:3], function(i) max(i$sd_mc)))
      max_y<-max(sapply(eff_frontier_list[2:3], function(i) max(i$mu_ub)))
      plot(eff_frontier_list[[1]]$frontier_sd, eff_frontier_list[[1]]$frontier_mean, 
           type="l", col=col_vector[1], xlim=c(min_x-0.0005, max_x+0.001), ylim=c(0, max_y+0.00001), 
           xlab=risk_measure, ylab=expression(paste(mu, "-MC")), main=title)
      if(ci_background){
        for(i in 2:length(eff_frontier_list)){
          polygon(c(eff_frontier_list[[i]]$sd_mc, rev(eff_frontier_list[[i]]$sd_mc)), 
                  c(eff_frontier_list[[i]]$mu_lb, rev(eff_frontier_list[[i]]$mu_ub)), 
                  col="grey90", border=F)
        }
      }
      lines(eff_frontier_list[[1]]$frontier_sd, eff_frontier_list[[1]]$frontier_mean, 
            type="l", col=col_vector[1])
      for(i in 2:length(eff_frontier_list)){
        lines(eff_frontier_list[[i]]$sd_mc, eff_frontier_list[[i]]$mu_mc, col=paste0(col_vector[i], 3))
        lines(eff_frontier_list[[i]]$sd_mc, eff_frontier_list[[i]]$mu_lb, col=col_vector[i], lty=2)
        lines(eff_frontier_list[[i]]$sd_mc, eff_frontier_list[[i]]$mu_ub, col=col_vector[i], lty=2)
      }
    }
  }
  else{
    if(risk_measure=="ES95"){
      min_x<-min(sapply(eff_frontier_list, function(i) min(i$ES95_mc)))
      max_x<-max(sapply(eff_frontier_list, function(i) max(i$ES95_mc)))
      max_y<-max(sapply(eff_frontier_list, function(i) max(i$mu_ub)))
      plot(eff_frontier_list[[1]]$ES95_mc, eff_frontier_list[[1]]$mu_mc, 
           type="l", col=col_vector[1], xlim=c(min_x-0.0005, max_x+0.0001), ylim=c(0, max_y+0.00001), 
           xlab=risk_measure, ylab=expression(paste(mu, "-MC")), main=title)
      if(ci_background){
        for(i in 1:length(eff_frontier_list)){
          polygon(c(eff_frontier_list[[i]]$ES95_mc, rev(eff_frontier_list[[i]]$ES95_mc)), 
                  c(eff_frontier_list[[i]]$mu_lb, rev(eff_frontier_list[[i]]$mu_ub)), 
                  col="grey90", border=F)
        }
        for(i in 1:length(eff_frontier_list)){
          lines(eff_frontier_list[[i]]$ES95_mc, eff_frontier_list[[i]]$mu_mc, col=paste0(col_vector[i], 3))
          lines(eff_frontier_list[[i]]$ES95_mc, eff_frontier_list[[i]]$mu_lb, col=col_vector[i], lty=2)
          lines(eff_frontier_list[[i]]$ES95_mc, eff_frontier_list[[i]]$mu_ub, col=col_vector[i], lty=2)
        }
      }
      for(i in 1:length(eff_frontier_list)){
        lines(eff_frontier_list[[i]]$ES95_mc, eff_frontier_list[[i]]$mu_mc, col=paste0(col_vector[i], 3))
      }
    }
    else if(risk_measure=="ES99"){
      min_x<-min(sapply(eff_frontier_list, function(i) min(i$ES99_mc)))
      max_x<-max(sapply(eff_frontier_list, function(i) max(i$ES99_mc)))
      max_y<-max(sapply(eff_frontier_list, function(i) max(i$mu_ub)))
      plot(eff_frontier_list[[1]]$ES99_mc, eff_frontier_list[[1]]$mu_mc, 
           type="l", col=col_vector[1], xlim=c(min_x-0.0005, max_x+0.0001), ylim=c(0, max_y+0.00001), 
           xlab=risk_measure, ylab=expression(paste(mu, "-MC")), main=title)
      if(ci_background){
        for(i in 1:length(eff_frontier_list)){
          polygon(c(eff_frontier_list[[i]]$ES99_mc, rev(eff_frontier_list[[i]]$ES99_mc)), 
                  c(eff_frontier_list[[i]]$mu_lb, rev(eff_frontier_list[[i]]$mu_ub)), 
                  col="grey90", border=F)
        }
      }
      for(i in 1:length(eff_frontier_list)){
        lines(eff_frontier_list[[i]]$ES99_mc, eff_frontier_list[[i]]$mu_mc, col=paste0(col_vector[i], 3))
        lines(eff_frontier_list[[i]]$ES99_mc, eff_frontier_list[[i]]$mu_lb, col=col_vector[i], lty=2)
        lines(eff_frontier_list[[i]]$ES99_mc, eff_frontier_list[[i]]$mu_ub, col=col_vector[i], lty=2)
      }
    }
    else if(risk_measure=="var"){
      min_x<-min(sapply(eff_frontier_list, function(i) min(i$sd_mc)))
      max_x<-max(sapply(eff_frontier_list, function(i) max(i$sd_mc)))
      max_y<-max(sapply(eff_frontier_list, function(i) max(i$mu_ub)))
      plot(eff_frontier_list[[1]]$sd_mc, eff_frontier_list[[1]]$mu_mc, 
           type="l", col=col_vector[1], xlim=c(min_x-0.0005, max_x+0.0001), ylim=c(0, max_y+0.00001), 
           xlab=risk_measure, ylab=expression(paste(mu, "-MC")), main=title)
      if(ci_background){
        for(i in 1:length(eff_frontier_list)){
          polygon(c(eff_frontier_list[[i]]$sd_mc, rev(eff_frontier_list[[i]]$sd_mc)), 
                  c(eff_frontier_list[[i]]$mu_lb, rev(eff_frontier_list[[i]]$mu_ub)), 
                  col="grey90", border=F)
        }
      }
      for(i in 1:length(eff_frontier_list)){
        lines(eff_frontier_list[[i]]$sd_mc, eff_frontier_list[[i]]$mu_mc, col=paste0(col_vector[i], 3))
        lines(eff_frontier_list[[i]]$sd_mc, eff_frontier_list[[i]]$mu_lb, col=col_vector[i], lty=2)
        lines(eff_frontier_list[[i]]$sd_mc, eff_frontier_list[[i]]$mu_ub, col=col_vector[i], lty=2)
      }
    }
  }
}

