#'@title Mean reversion screen
#'
#'@param data List of price data
#'@param ticker ticker selected
#'@export
mean_reversion <- function(data, ticker){
  df = data[[ticker]]
  df = df[complete.cases(df)]
  n = nrow(df)
  if(n <= 150){
    return(NA)
  }
  HLC = HLC(df)
  sma150 = SMA(Cl(df), n=150)
  adx7 = ADX(HLC, n=7, matype='EMA')
  atr10 = ATR(HLC, n=10, maType='EMA')
  rsi3 = RSI(Cl(df), n=3)

  df = merge(df, sma150)
  df = merge(df, adx7$ADX)
  df = merge(df, atr10$atr)
  df = merge(df, rsi3)
  df$atr = df$atr / Cl(df)
  df$close = Cl(df)
  keep_cols = c("SMA","ADX","atr","rsi","close")
  df = df[, keep_cols]
  names(df)[1:4] = c("sma150", "adx7", "atr10", "rsi3")
  df = tail(df, 1)

  dt = as.data.table(df)
  dt[, ticker:=ticker]
  setnames(dt, "index", "date")

  dt
}

#'@title Long term trend following strategy based on weekly rebalancing
#'
#'@param data List of price data
#'@param ticker ticker selected
#'@param n_return Number of days to compute return.
#'@export
weekly_rotation <- function(data, ticker, n_return=200){
  df = na.omit(data[[ticker]])
  n = nrow(df)

  if(n < n_return){
    return(NA)
  }

  SPY = data[['SPY']]
  sma200 = SMA(Ad(SPY), n=n_return) * 0.98

  avg_vol = mean(tail(Vo(df), 20)) / 1e6
  close = Ad(df)
  rsi = RSI(close, n=3)
  R_200 = close[n,1][[1]] / close[n-n_return][[1]] - 1

  data.table(ticker, SPY=tail(Ad(SPY), 1)[[1]],
             `SPY-200-SMA`=tail(sma200,1)[[1]],
             rsi=tail(rsi, 1)[[1]],
             close=tail(close, 1)[[1]], R_200, avg_vol)
}


#'@title Long term trend following strategy based on stocks with momentum
#'
#'@param data List of price data
#'@param ticker ticker selected
#'@param n_return Number of days to compute a "long term" return
#'@export
long_trend_high_momentum <- function(data, ticker, n_return=200){
  #print(ticker)
  df = data[[ticker]]
  df = na.omit(df)
  df = try(adjustOHLC(df, use.Adjusted=TRUE), silent=T)

  if(class(df)[1] == 'try-error') return(NA)
  if(nrow(df) < 201) return(NA)

  df$dv = Cl(df) * Vo(df)
  close = Cl(df)
  last_close = tail(close, 1)[[1]]
  avg_volume = mean(tail(df$dv, 20)) / 1000000

  df$sma25 = SMA(close, n=25)
  df$sma50 = SMA(close, n=50)
  df_last = tail(df, 1)

  # filters
  test_sma25_sma50 = df_last$sma25[[1]] >= df_last$sma50[[1]]
  test_close_higher5 = ifelse(last_close >= 5, TRUE, FALSE)
  test_volume = ifelse(avg_volume >= 50, TRUE, FALSE)

  return_200d = tail(close, 1)[[1]] / tail(close, n_return)[[1]]-1
  #return_200d = tail(ROC(close, n=200, type='discrete'), 1)[[1]]
  atr = tail(ATR(HLC(df), n=20)$atr, 1)[[1]]
  # Stop loss: 5 * ATR(20)
  stop = last_close - atr * 5

  data.table(ticker, close=last_close,
             return_200d, stop, atr, avg_volume,
             test_sma25_sma50,
             test_close_higher5,
             test_volume)
}


#'@title Long term trend following strategy based on stocks low volatility
#'
#'@param data List of price data
#'@param ticker ticker selected
#'@param n_return Number of days to compute a "long term" return
#'@export
long_trend_low_vol <- function(data, ticker, n_return=200){
  #print(ticker)
  df = data[[ticker]]
  df = na.omit(df)
  df = try(adjustOHLC(df, use.Adjusted=TRUE), silent=T)

  if(class(df)[1] == 'try-error') return(NA)
  if(nrow(df) < 201) return(NA)

  df$dv = Cl(df) * Vo(df)
  close = Cl(df)
  sma200 = tail(SMA(close, n=200), 1)[[1]]

  R = ROC(tail(close, 252*3), n=1, type='discrete')
  volatility = StdDev.annualized(R)[[1]]
  last_close = tail(close, 1)[[1]]
  avg_volume = mean(tail(df$dv, 20)) / 1000000

  # filters
  #test_close_higher5 = ifelse(last_close >= 5, TRUE, FALSE)
  #test_volume = ifelse(avg_volume >= 50, TRUE, FALSE)

  return_200d = tail(close, 1)[[1]] / tail(close, n_return)[[1]]-1
  #return_200d = tail(ROC(close, n=200, type='discrete'), 1)[[1]]
  rsi4 = tail(RSI(close, n=4), 1)[[1]]
  atr = tail(ATR(HLC(df), n=40)$atr, 1)[[1]]
  # Stop loss: 5 * ATR(20)
  stop = last_close - atr * 1.5

  data.table(ticker, close=last_close, sma200, rsi4,
             return_200d, volatility, stop, atr,
             avg_volume)

}



#'@title Screen stocks with a "base" patter
#'@description The idea is to look for stocks in a base for the past few months.
#'    I also compute volatility statistics that are interesting to filter stocks further.
#'@param data List of price data
#'@param ticker ticker selected
#'@param trailing_days Number of days of the base
#'@export
base_stats <- function(data, ticker, trailing_days=8*21){
  #print(ticker)
  df = data[[ticker]]
  df = na.omit(df)
  df = try(adjustOHLC(df, use.Adjusted=TRUE), silent=T)

  if(class(df)[1] == 'try-error') return(NA)
  if(nrow(df) < 201) return(NA)

  df$dv = Cl(df) * Vo(df)
  avg_volume = mean(tail(df$dv, 20)) / 1000000

  close = Cl(df)
  names(close) = 'close'

  hlc = HLC(df)
  atr = ATR(hlc, n=20)

  df_close = cbind(close, atr$atr)
  df_close = df_close[complete.cases(df_close)]
  df_close$atr_pct = df_close$atr/df_close$close

  start_vol_date = Sys.Date()-trailing_days

  atr_pct = df_close[paste0(start_vol_date, "/"), 'atr_pct']

  fit_atr = try(lm(atr_pct$atr_pct ~ log(as.numeric(index(atr_pct)))), silent=TRUE)

  if(class(fit_atr) == "try-error"){
    coef_atr=0
  } else {
    smr_atr = summary(fit_atr)
    coef_atr=coefficients(smr_atr)[2,1]
  }

  natr = nrow(atr_pct)
  middleatr = round(natr/2)
  vol_start = mean(atr_pct[1:20,])
  vol_middle = mean(atr_pct[(middleatr-10):(middleatr+10),])
  vol_end = mean(atr_pct[(natr-20):natr,])

  vol_end_middle = vol_end - vol_middle
  vol_middle_start = vol_middle - vol_start
  vol_end_start = vol_end - vol_start

  ret_period = tail(close, 1)[[1]] / tail(close, natr)[[1]]-1

  data.table(ticker,
             close=tail(close, 1)[[1]],
             avg_volume,
             ret_period,
             vol_start, vol_middle, vol_end,
             vol_end_middle, vol_middle_start, vol_end_start,
             coef_atr=coef_atr)
}
