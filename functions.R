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

# Data loading and manipulation------------------------------------------------------------
#Function to load from SQL
sql_load_tickers <- function(tickers, from, to=Sys.Date(), convert_to_EUR){
  con1  <-  odbcDriverConnect('driver={SQL Server};
    server=production_sql;
    database=CTAManager;
    trusted_connection=true')
  tab_big <- list()
  for(i in seq_along(tickers)){
    if(tickers[i]=="Div i14 10%"){
      tab  <-  as.data.table(sqlQuery(con1, query =
                                          paste("select Date, NAV as Price from vCTAM_FactsheetData n
                                                where n.Name = 'Div i14 10%' and Date between ", paste0("('", from, "')"), 
                                                "and", paste0("('", to, "')"),  "order by 1"), stringsAsFactors=F))
    }
    else if(tickers[i]=="IMF"){
      tab  <-  as.data.table(sqlQuery(con1, query = paste("select g.Date, g.Value as Price from CTAM_GAV g 
                        join CTAM_GAVType gt on gt.ID = g.Type_ID
                        join CTAM_Product p on p.ID = g.Product_ID
                        where p.Name = 'IMF' and gt.Name = 'Official (Index)' and g.Date between ", paste0("('", from, "')"), 
                        "and", paste0("('", to, "')"), sep="" )))
    }
    else{
      if(convert_to_EUR){
        tab <- as.data.table(sqlQuery(con1, query =
                                            paste("select h.Date, h.Price*r.Rate as Price
                                                  from SecurityData.dbo.vSD_Security s
                                                  join SecurityData.dbo.vSD_History h on s.ID = h.Security_ID
                                                  join dbo.vSMN_XRateTopProvider r on h.Date = r.Date and s.Currency=r.Code_From
                                                  where s.Ticker in ", paste0("('", tickers[i], "')"), 
                                                  "and h.Date between ", paste0("('", from, "')"), "and ", paste0("('", to, "')"),
                                                  "and r.Code_To='EUR'"), stringsAsFactors=F))
      }
      else{
        tab <- as.data.table(sqlQuery(con1, query =
                                            paste("select h.Date, h.Price from SecurityData.dbo.vSD_Security s",  
                                                  "join SecurityData.dbo.vSD_History h on s.ID = h.Security_ID", 
                                                  "where s.Ticker in ", paste0("('", tickers[i], "')"), 
                                                  "and Date between ", paste0("('", from, "')"), "and ", paste0("('", to, "')")), 
                                          stringsAsFactors=F))
      }
    }
    tab[, Date:=as.Date(Date)]
    tab[, Instrument:=tickers[i]]
    if(nrow(tab)>0){
      tab_big<-rbindlist(list(tab_big, tab)) 
    }
  }
  names(tab_big)[2] <- "Close"
  tab_big
}

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

#Function to change from daily to monthly data
daily_to_mthly <- function(dtab){
  names <- colnames(dtab)
  dtab <- copy(dtab)
  month <- format(dtab$Date, "%m")
  month_s <- shift(month, type = "lead")
  change <- month!=month_s
  dtab[,month_change:=change]
  dtab <- dtab[change==TRUE, names, with=F]
  dtab
}


# Backtesting -------------------------------------------------------------
#Backtester
backtest_simp <- function(prices_dtab, weights_tab, rebal_freq, start_cap=1, 
                          from, to=prices_dtab[,max(Date)]){
  if(nrow(weights_tab)!=prices_dtab[,length(unique(Instrument))]){
    stop("There is not a weight assigned to each instrument")
  }
  prices_dtab <- copy(prices_dtab)
  prices_dtab <- prices_dtab[Date>=from & Date<=to]
  prices_dtab <- dcast(prices_dtab, formula = Date~Instrument, value.var = "Close", fill = NA)
  #Safety check
  setorder(prices_dtab, Date)
  #Fill NAs from holidays with price from the day before
  prices_dtab[, (names(prices_dtab)[-1]) := lapply(.SD, nafill, type = "locf"), .SDcols = -1]
  #Daily returns
  ret_dtab <- copy(prices_dtab)
  ret_dtab[, (names(ret_dtab)[-1]) := lapply(.SD, function(x) c(1, exp(diff(log(x))))), .SDcols = -1]
  #Fill NAs with 1s as this implies price changes of zero
  ret_dtab <- ret_dtab[, lapply(.SD, function(x) replace(x, is.na(x), 1))]
  #Ensure column order matches weight tab
  setcolorder(ret_dtab, neworder = c("Date", weights_tab[,Instrument]))
  year <- as.integer(format(ret_dtab$Date, "%Y"))
  month <- as.integer(format(ret_dtab$Date, "%m"))
  if(rebal_freq=="Yearly"){
    year_s <- shift(year)
    rebal <- year!=year_s
  }
  else if(rebal_freq=="Semesterly"){
    semester <- fifelse(month<=6, 1, 2)
    semester_s <- shift(semester)
    rebal <- semester!=semester_s
  }
  else if(rebal_freq=="Quarterly"){
    quarter <- as.numeric(cut(as.numeric(month), breaks = c(0, 3, 6, 9, 12), labels = c(1, 2, 3, 4)))
    quarter_s <- shift(quarter)
    rebal <- quarter!=quarter_s
  }
  else if(rebal_freq=="Monthly"){
    month_s <- shift(month)
    rebal <- month!=month_s
  }
  else if(rebal_freq=="Daily"){
    rebal_true <- rep(TRUE, nrow(ret_dtab))
  }
  rebal[is.na(rebal)] <- TRUE
  rebal_true <- which(rebal)
  pl <- numeric(nrow(ret_dtab))
  for(i in seq_along(rebal_true)){
    start_index <- rebal_true[i]
    end_index <- ifelse(is.na(rebal_true[i+1]-1), nrow(ret_dtab), rebal_true[i+1]-1)
    subdt <- ret_dtab[start_index:end_index, ][,-1]
    subdt_cum <- apply(subdt, 2, cumprod)
    pl[start_index:end_index] <- rowSums(t(t(subdt_cum)*weights_tab[,Weight]*start_cap), na.rm = T)
    start_cap <- pl[end_index]
  }
  result <- ret_dtab[, (names(ret_dtab)[-1]) := lapply(.SD, cumprod), .SDcols = -1]
  result[,portfolio_pl:=pl]
  result
}

#Backtester with lot sizes, transaction costs and input from InstrumentData
backtest <- function(inst_dtab, contract_dtab=NULL, begin=min(inst_dtab$Date), end=max(inst_dtab$Date),
                     start_equity_capital, margin_req=1, invest_weight=0.95,
                     contract_no=1, transaction_costs=T, slippage=T, stoch_slippage, 
                     slip_prob_pos = NULL, slip_source=c("sql", "tblox"),
                     data_manipulation_function, decision_function){
  capital <- start_equity_capital
  leverage_factor <- 1/margin_req
  inst_dtab <- copy(inst_dtab)
  inst_dtab <- inst_dtab[Date>=begin & Date<=end]
  #Restriction of time to maturity 
  inst_dtab <- inst_dtab[ContractNo==contract_no]
  if(transaction_costs==F){
    inst_dtab[,`:=`(Transcosts=0)]
  }
  if(slippage==F){
    inst_dtab[,`:=`(Slippage=0, Slippage_roll=0)]
  }
  else{
    if(slip_source=="sql"){
      inst_dtab[,Slippage_roll:=Slippage/2]
    }
    else if(slip_source=="tblox"){
      inst_dtab[,`:=`(Transcosts=0, Slippage=Slippage/2, 
                      Slippage_roll=Slippage_roll/2)]
    }
  }
  if(stoch_slippage){
    slip_prob_neg <- 1-slip_prob_pos
    inst_dtab[, `:=`(Slippage=Slippage*sample(x = c(-1, 1), size = .N, replace = T, 
                                              prob = c(slip_prob_pos, slip_prob_neg)),
                     Slippage_roll=Slippage_roll*sample(x = c(-1, 1), size = .N, replace = T, 
                                                        prob = c(slip_prob_pos, slip_prob_neg)))]
  }
  #Filter days out that do not have enough info for all commodities.
  #Criterion: delete days where the number of available commodities was less than the day before.
  commodity_count  <-  inst_dtab[, .(commodity_count = uniqueN(Commodity)), by = Date]
  commodity_count[, prev_max := cummax(commodity_count)]
  commodity_count  <-  commodity_count[commodity_count >= prev_max]
  inst_dtab <- inst_dtab[Date %in% commodity_count[,Date]]
  #Necessary data manipulations for signal generation
  inst_dtab <- data_manipulation_function(prices_dtab = inst_dtab, contract_dtab=contract_dtab)
  #Create columns to assign memory
  inst_dtab[, `:=`(deltaprice_Open=c(0, diff(Open))*pointvalue, deltaprice_Close=c(0, diff(Close))*pointvalue), 
            by=.(Commodity)]
  #Shifted Close to compute Close-to-Open P&L from t-1 to t
  inst_dtab[, `:=`(Close_lag=shift(Close), UnadjustedClose_lag=shift(UnadjustedClose)), by=Commodity]
  #CtO and intraday P&L
  inst_dtab[, `:=`(overnight_pl=(Open-Close_lag)*pointvalue, intraday_pl=(Close-Open)*pointvalue), by=Commodity]
  #Rollday logicals
  inst_dtab[, rollday:=c(0, diff(DeliveryMonth)), by=Commodity]
  inst_dtab[, rolldayT:=shift(rollday, -1), by=Commodity]
  #Create column indicating whether a position is open
  inst_dtab[, `:=`(open_pos_ind=0, direction=0)]
  #Create column of portfolio weights
  inst_dtab[,port_weights:=0]
  #Column of transaction costs
  inst_dtab[,tcosts:=0]
  #Order alphabetically by date for safety
  inst_dtab<-inst_dtab[order(Date, Commodity)]
  dates <- inst_dtab[!is.na(signal_open), unique(Date)]
  inv_capital <- numeric(length(dates))
  capital_short <- numeric(length(dates))
  capital_long <- numeric(length(dates))
  cash_position <- numeric(length(dates))
  equity_capital <- rep(start_equity_capital, length(dates))
  #Equity Capital valued with second method
  transaction <- numeric(length(dates))
  #Capital of zero for first day in order to avoid NAs
  inst_dtab[Date==dates[1], capital_commodity:=0]
  pb <- txtProgressBar(min = 1, max = length(dates)+1, style = 3)
  # seq_along(dates)[-1]
  for(i in seq_along(dates)[-1]){
    result <- tryCatch({
      day_tab <- inst_dtab[Date==dates[i]]
      decision_tab <- inst_dtab[Date==dates[i-1]]
      decision_vector <- decision_function(decision_tab=decision_tab)
      open_trade <- decision_vector[["Open Trade"]]
      close_trade <- decision_vector[["Close Trade"]]
      long_instruments <- decision_vector[["Long Instruments"]]
      short_instruments <- decision_vector[["Short Instruments"]]
      weight <- decision_vector[["Weight"]]
      stop_losses <- decision_vector[["Stop Loss"]]
      if(i==2){
        #Fill port_weights column with corresponding weight if commodity long, short or neutral
        inst_dtab[Date==dates[i], `:=`(open_pos=fifelse(Commodity %in% open_trade, 1, 0),
                                       open_pos_ind=fifelse(Commodity %in% open_trade, 1, 0),
                                       close_pos=0)]
        inst_dtab[Date==dates[i], port_weights:=fifelse(Commodity %in% open_trade, weight, 0)]
        inst_dtab[Date==dates[i], direction:=fifelse(Commodity %in% long_instruments, 1, 
                                                     fifelse(Commodity %in% short_instruments, -1, 0))]
        #Starting notional capital at the beginning of each trade for perf attr purposes 
        #assuming some margin factor
        inst_dtab[Date==dates[i], init_cap:=fifelse(Commodity %in% open_trade, invest_weight*weight*capital, 0)]
        #Determine number of contracts bought per commodity
        inst_dtab[Date==dates[i], contracts:=floor((init_cap*FX_rate)/decision_tab[,UnadjustedClose*pointvalue]), by=Date]
        inst_dtab[Date==dates[i], `:=`(cont_adj=inst_dtab[Date==dates[i], contracts],
                                       cont_adj_abs=inst_dtab[Date==dates[i], contracts],
                                       new_contracts=inst_dtab[Date==dates[i], contracts])]
        inst_dtab[Date==dates[i], tcosts:= -(cont_adj_abs*(Slippage+Transcosts))/FX_rate]
        inst_dtab[Date==dates[i], pl_commodity:= contracts*(intraday_pl/FX_rate)*direction]
        #P&L with incl. fees 
        inst_dtab[Date==dates[i], pl_commodity_fees:= pl_commodity+tcosts]
        #SynthCVal to be used to MtM total value of positions
        inst_dtab[Date==dates[i], SynthCVal:=UnadjustedOpen*pointvalue+intraday_pl*direction]
        #Invested capital at the beginning of the day to compute cash position
        inst_dtab[Date==dates[i], capital_commodity:= (UnadjustedOpen*pointvalue/FX_rate)*contracts*margin_req]
        capital_open_pos <- inst_dtab[Date==dates[i], sum(capital_commodity)]
        transaction[i] <- inst_dtab[Date==dates[i], sum(tcosts)]
        cash_position[i] <- equity_capital[i-1]-capital_open_pos + transaction[i]
        #Update invested capital with intraday P&L
        inst_dtab[Date==dates[i], capital_commodity:= capital_commodity+pl_commodity]
        inv_capital[i] <- inst_dtab[Date==dates[i], sum(capital_commodity)]
        capital_long[i] <- day_tab[direction==1, sum(capital_commodity)]
        capital_short[i] <- day_tab[direction==-1, sum(capital_commodity)]
      }
      else{
        if(nrow(decision_tab)<nrow(inst_dtab[Date==dates[i]])){
          decision_tab<-merge(decision_tab, inst_dtab[Date==dates[i], .(Commodity)], all.y=T, by="Commodity")
          inst_dtab[Date==dates[i], pl_commodity:=fifelse(is.na(pl_commodity), 0, pl_commodity)]
        }
        inst_dtab[, `:=`(prev_open_ind=shift(open_pos_ind), prev_contracts=shift(contracts), 
                         prev_direction=shift(direction), prev_weight=shift(port_weights)), by=Commodity]
        inst_dtab[Date==dates[i], `:=`(contracts=fifelse(prev_open_ind==1, prev_contracts, 0, na = 0),
                                       direction=fifelse(prev_open_ind==1, prev_direction, 0, na = 0))]
        inst_dtab[Date==dates[i], pl_commodity:=(contracts*direction*overnight_pl)/FX_rate]
        inst_dtab[Date==dates[i], SynthCVal:=decision_tab[,SynthCVal]+overnight_pl*direction]
        #Fill port_weights column with corresponding weight if commodity long, short or neutral for next period
        inst_dtab[Date==dates[i], `:=`(open_pos=fifelse(Commodity %in% open_trade, 1, 0),
                                       close_pos=fifelse(Commodity %in% close_trade, 1, 0),
                                       port_weights=fifelse(Commodity %in% open_trade, weight, prev_weight))]
        inst_dtab[Date==dates[i], open_pos_ind:=fifelse(Commodity %in% open_trade, 1,
                                                        fifelse(Commodity %in% close_trade, 0, prev_open_ind))]
        inst_dtab[Date==dates[i], direction:=fifelse(Commodity %in% long_instruments, 1, 
                                                     fifelse(Commodity %in% short_instruments, -1, prev_direction))]
        #Starting capital at the beginning of each trade for perf attr purposes
        inst_dtab[Date==dates[i], init_cap:=fifelse(Commodity %in% open_trade, invest_weight*weight*equity_capital[i-1], 0)]
        #Exception used for when new commodities enter the data set
        #Determine number of contracts bought per commodity
        inst_dtab[Date==dates[i] & Commodity %in% open_trade,
                  contracts:=fifelse(is.na(floor((init_cap*FX_rate)/decision_tab[Commodity %in% open_trade, UnadjustedClose*pointvalue])), 0,
                                     floor((init_cap*FX_rate)/decision_tab[Commodity %in% open_trade, UnadjustedClose*pointvalue]))]
        inst_dtab[Date==dates[i] & Commodity %in% close_trade, contracts:=0]
        #Compute  differences in contracts to compute new cash position, transaction costs and P&L
        inst_dtab[Date==dates[i], cont_adj:=contracts-prev_contracts]
        #Differentiate between commodities where trade direction changed
        inst_dtab[Date==dates[i], cont_adj_abs:=abs(cont_adj)]
        inst_dtab[Date==dates[i], tcosts:=-((cont_adj_abs * (Slippage+Transcosts))/FX_rate)]
        inst_dtab[Date==dates[i], capital_commodity:=fifelse(Commodity %in% open_trade, 
                                                             (UnadjustedOpen*pointvalue/FX_rate)*contracts*margin_req,
                                                             (SynthCVal/FX_rate)*contracts*margin_req, na = 0)]
        #Tcosts for open positions that have a roll day
        inst_dtab[Date==dates[i] & capital_commodity & rollday!=0 & open_pos_ind==1 & prev_open_ind==1, 
                  tcosts:=(-2*contracts*(Transcosts+Slippage_roll))/FX_rate]
        inst_dtab[Date==dates[i], pl_commodity:= pl_commodity+contracts*(intraday_pl/FX_rate)*direction]
        inst_dtab[Date==dates[i], pl_commodity_fees:= pl_commodity+tcosts]
        transaction[i] <- inst_dtab[Date==dates[i], sum(na.omit(tcosts))]
        #Update cash position in case signal generated transactions are made
        if(nrow(inst_dtab[Date==dates[i] & (open_pos==1 | close_pos==1),])>0){
          #New invested capital is difference between income of closed positions and that of open(ed) positions
          capital_open_pos <- inst_dtab[Date==dates[i] & open_pos==1, sum(na.omit(capital_commodity))]
          income_closed_pos <- inst_dtab[Date==dates[i] & close_pos, 
                                         sum(na.omit((SynthCVal/FX_rate)*cont_adj_abs*margin_req))]
          delta_inv_capital <-  capital_open_pos-income_closed_pos
          cash_position[i] <- cash_position[i-1] - delta_inv_capital + transaction[i]
          inst_dtab[Date==dates[i] & Commodity %in% open_trade, SynthCVal:=UnadjustedOpen*pointvalue]
          inst_dtab[Date==dates[i] & Commodity %in% close_trade, SynthCVal:=0]
        }
        #Update cash position in case roll day transactions are made
        else{
          cash_position[i] <- cash_position[i-1] + transaction[i]
          # cash_position[i] <- cash_position[i-1]
        }
        #Change capital_commodity to account for intraday P&L
        inst_dtab[Date==dates[i] & open_pos_ind, SynthCVal:=SynthCVal+intraday_pl*direction]
        inst_dtab[Date==dates[i], capital_commodity:=capital_commodity+(intraday_pl/FX_rate)*contracts*direction]
        inv_capital[i] <- inst_dtab[Date==dates[i], sum(na.omit(capital_commodity))]
        capital_long[i] <- inst_dtab[Date==dates[i] & direction==1, sum(na.omit(capital_commodity))]
        capital_short[i] <- inst_dtab[Date==dates[i] & direction==-1, sum(na.omit(capital_commodity))]
      }
      equity_capital[i] <- inv_capital[i] + cash_position[i]
      inst_dtab[Date==dates[i-1], stop_loss:=na.omit(stop_losses[inst_dtab[Date==dates[i]], on=.(Commodity), stop_loss])]
      inst_dtab[Date==dates[i], stop_loss_s:=stop_losses[inst_dtab[Date==dates[i]], on=.(Commodity), stop_loss]]
      #Check if any commodity needs to be rolled AND it is not rebalancing day
    }, error = function(e) {
      message(paste("Error in iteration", i, ":", e))
      break
    })
    setTxtProgressBar(pb, i)
  }
  close(pb)
  #P&L data table by Commodity and whole portfolio
  pl_dtab <- dcast(inst_dtab, Date~Commodity, value.var = "capital_commodity")
  #Replace NAs
  pl_dtab <- pl_dtab[, (names(pl_dtab)[-1]):=lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols=(names(pl_dtab)[-1])]
  if(nrow(pl_dtab)!=length(capital_long)){
    pl_dtab<-pl_dtab[-(1:(nrow(pl_dtab)-length(capital_long)))]
  }
  pl_dtab[,`:=`(capital_long=capital_long, capital_short=capital_short, 
                inv_capital=inv_capital, cash_position=cash_position, 
                transaction=transaction, equity_capital=equity_capital)]
  pl_dtab[1, equity_capital:=start_equity_capital]
  #Positioning data table by commodity
  pos_dtab <- dcast(inst_dtab, Date~Commodity, value.var = "direction")
  pos_dtab <- pos_dtab[, (names(pos_dtab)[-1]) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
                       .SDcols = names(pos_dtab)[-1]]
  # #Performance attribution data table by commodity
  perf_dtab <- dcast(inst_dtab, Date~Commodity, value.var = "pl_commodity_fees")
  perf_dtab <- perf_dtab[, (names(perf_dtab)[-1]) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
                         .SDcols = names(perf_dtab)[-1]]
  if(nrow(perf_dtab)!=length(equity_capital)){
    perf_dtab <- perf_dtab[-(1:(nrow(perf_dtab)-length(equity_capital))),]
  }
  CumprodDaily <- cumprod(c(1, exp(diff(log(equity_capital)))))
  perf_dtab <- perf_dtab[, (names(perf_dtab)[-1]) := lapply(.SD, function(x) x / shift(equity_capital)),
                         .SDcols = names(perf_dtab)[-1]]
  perf_dtab <- perf_dtab[,(names(perf_dtab)[-1]) := lapply(.SD, function(x) x * shift(CumprodDaily)),
                         .SDcols = names(perf_dtab)[-1]]
  perf_attr <- sum(colSums(na.omit(perf_dtab[,-1])))
  tot_ret <- last(equity_capital)/first(equity_capital)-1 
  tot_ret-perf_attr
  # #Perf attr of long/short portfolios
  attr_long <- inst_dtab[direction==1, .(long=sum(pl_commodity_fees)), by="Date"]
  attr_short <- inst_dtab[direction==-1,.(short=sum(pl_commodity_fees)), by="Date"]
  attr_neutral <- inst_dtab[direction==0,.(neutral=sum(pl_commodity_fees)), by="Date"]
  ls_attr <- merge(attr_long, attr_short, by = "Date", all = T)
  ls_attr <- merge(ls_attr, attr_neutral, by="Date", all=T)
  ls_attr[is.na(ls_attr)] <- 0
  if(nrow(perf_dtab)!=length(equity_capital)){
    ls_attr <- ls_attr[-(1:(nrow(ls_attr)-length(equity_capital))),]
  }
  ls_attr <- ls_attr[, (names(ls_attr)[-1]) := lapply(.SD, function(x) x / equity_capital),
                     .SDcols = names(ls_attr)[-1]]
  ls_attr <- ls_attr[,(names(ls_attr)[-1]) := lapply(.SD, function(x) x * c(1, exp(diff(log(equity_capital))))),
                     .SDcols = names(ls_attr)[-1]]
  sum(colSums(na.omit(ls_attr[,-1])))
  output_list <- list(inst_dtab, pl_dtab, pos_dtab, perf_attr, perf_dtab, ls_attr)
  names(output_list) <- c("long_dtab", "pl_dtab", "pos_dtab", "perf_attr", "perf_dtab", "ls_attr")
  # output_list <- list(inst_dtab, pl_dtab)
  # names(output_list) <- c("long_dtab", "pl_dtab")
  output_list
}

#Strategy summary statistics
summary_table <- function(returns, cor_to=NULL, time_unit_scaling, convert_returns=FALSE){
  dtab <- returns[,-1]
  if(convert_returns){
    dtab <- apply(dtab, MARGIN = 2, FUN = function(x) c(1,(exp(diff(log(x))))))
  }
  asset_names <- colnames(dtab)
  tot_return <- apply(dtab, MARGIN = 2, prod)-1
  cagr <- (tot_return+1)^(time_unit_scaling/nrow(dtab))-1
  ann_sd <- apply(dtab, MARGIN = 2, sd)*sqrt(time_unit_scaling)
  risk_to_return <- cagr/ann_sd
  max_dd <- apply(dtab, MARGIN = 2, FUN = function(x) f_dd(x-1))
  if(!is.null(cor_to)){
    cor_smn <- apply(dtab, MARGIN = 2, FUN = function(x) cor(x, returns[, cor_to, with=F]))
    sumry <- data.table("Instrument"=asset_names, "Total Ret"=round(tot_return*100, 2),
                        "CAGR"=round(cagr*100, 2), "Ann. SD"=round(ann_sd*100, 2), 
                        "Return-to-Risk Ratio"=round(risk_to_return, 2), 
                        "Max DD"=round(max_dd*100, 2), 
                        "Korrelation zum SMN i14"=round(cor_smn, 2))
  }
  else{
    sumry <- data.table("Instrument"=asset_names, "Tot. Ret"=tot_return, "CAGR"=round(cagr*100, 2), 
                      "Ann. SD"=round(ann_sd*100, 2), 
                      "Return-to-Risk Ratio"=round(risk_to_return, 2), 
                      "Max DD"=round(max_dd*100, 2))
  }
  #cbind(, tot_return, cagr, ann_sd, max_dd)
  print(sumry)
}

cagr <- function(terminal_value, n, ann=T, as_percentage=T){
  if(ann){
    cagr <- terminal_value^(12/n)
  }
  else{
    cagr <- terminal_value^(1/n)
  }
  if(as_percentage){
    cagr <- (cagr-1)*100
  }
  as.numeric(cagr)
}

# Bootstrapping -----------------------------------------------------------
#Bootstrap
bootstrapped_dtab <- function(dtab, boot_cols, t=nrow(dtab), n=100, cum.prod=TRUE, convert_returns){
  returns <- dtab[, get(boot_cols)]
  if(convert_returns){
    if(class(returns)=="numeric"){
      returns <- c(1,(exp(diff(log(returns)))))
    }
    else{
      returns <- apply(returns, MARGIN = 2, FUN = function(x) c(1,(exp(diff(log(x))))))
    }
  }
  boot_matrix <- matrix(NA, nrow = t, ncol = n)
  boot_matrix <- sapply(1:n, function(i){
    returns[sample(1:length(returns), size = t, replace = T)]
  })
  if(cum.prod){
    boot_matrix <- apply(boot_matrix, 2, cumprod)
  }
  as.data.table(boot_matrix)
}

#Plotting of paths
plot_paths_boot <- function(boot_dt){
  boot_dt <- copy(boot_dt)
  terminals <- unlist(boot_dt[nrow(boot_dt),])
  q_5 <- which.min(abs(terminals-quantile(terminals, 0.05)))
  q_50 <- which.min(abs(terminals-quantile(terminals, 0.5)))
  q_95 <- which.min(abs(terminals-quantile(terminals, 0.95)))
  q_paths <- c(q_5, q_50, q_95)
  q_paths <- c(q_5, q_50, q_95)
  color_vector <- c(rep("grey80", ncol(boot_dt)-3), "red4", "red", "red4")
  colorder <- 1:ncol(boot_dt)
  colorder <- colorder[-(q_paths)]
  setcolorder(boot_dt, c(colorder, q_paths))
  index <- 1:nrow(boot_dt)
  par(mar=c(5.1, 4.1, 2, 8.1), xpd=TRUE)
  plot(index, unlist(boot_dt[,1]), type="l", col="grey70", 
       xlab="Time", ylab="Wealth", ylim=c(0.7, max(terminals)))
  for(i in 2:ncol(boot_dt)){
    lines(index, unlist(boot_dt[,i, with=FALSE]), type="l", col=color_vector[i])
  }
  legend("topright", inset = c(-0.18, 0.5), legend=c("Paths","5th and/n95th Percentile", "Median"),
         lty=1, col=c("grey70", "red4", "red"), cex=0.7, bg = "white")
  par(xpd=FALSE)
  grid(lty = 1,      
       col = "gray90", 
       lwd = 0.5) 
}

