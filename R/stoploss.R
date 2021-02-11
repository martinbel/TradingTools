#'@title Define a stop loss based on the ATR:
#'@description I use the following definition, with default parameters:
#'    Stop = Close - ATR(40) * multiple
#'
#'@param data List of price data xts objects
#'@param ticker Ticker
#'@param n_days Number of periods for the moving average
#'@param atr_multiple times I multiply the ATR to substract it from the current close price
#'@export
atr_stoploss <- function(data, ticker, n_days=40, atr_multiple=1.5){
  df = data[[ticker]]
  df = df[complete.cases(df)]
  n = nrow(df)
  if(n <= 150){
    return(NA)
  }
  HLC = HLC(df)
  atr = ATR(HLC, n=n_days, maType='EMA')

  df = merge(df, atr$atr)
  df$atr_pct = df$atr / Cl(df)
  df$close = Cl(df)
  df$stop = df$close - df$atr * atr_multiple
  keep_cols = c("close", "stop", "atr", "atr_pct")
  df = df[, keep_cols]
  df = tail(df, 1)

  dt = as.data.table(df)
  dt[, ticker:=ticker]
  setnames(dt, "index", "date")

  dt
}
