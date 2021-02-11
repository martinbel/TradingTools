#'@title Compute Keltner Bands
#'
#'@param x xts price data
#'@param n.ema Period of the EMA
#'@param n.atr Period of the ATR
#'@param multiplier Used to define the width of the band
#'@param maType Type of moving average
#'@export
KeltnerBands <- function(x, n.ema = 21, n.atr = 21, multiplier = 2, maType='EMA') {
  x <- try.xts(x, error = as.matrix)
  ma <- do.call(what = maType, args = list(Cl(x), n.ema))
  atr <- ATR(HLC(x), n = n.atr, maType = maType)$atr
  upper <- ma + multiplier * atr
  lower <- ma - multiplier * atr
  r <- cbind(lower, ma, upper)
  colnames(r) <- c("KC.lower", "KC.mid", "KC.upper")
  return(reclass(r,x))
}

#'@title compute KC/DC indicators
#'
#'@param xts price data
#'@export
compute_channels <- function(df){
  # Channels period
  n_high = 21
  n_low = 50

  # Donchian-Channel
  hl = cbind(Hi(df), Lo(df))
  dc_low = DonchianChannel(hl, n=n_low)
  dc_high = DonchianChannel(hl, n=n_high)

  dc = merge(dc_high[, "low"], dc_low[, "high"])

  # Kellner-Bands
  kb_low = KeltnerBands(df, n.ema=n_low, n.atr=n_low, multiplier = 2, maType="EMA")
  kb_high = KeltnerBands(df, n.ema=n_high, n.atr=n_high, multiplier = 2, maType="EMA")
  kb = merge(kb_low[, "KC.lower"], kb_high[, "KC.upper"])

  # lag indicators so they can be used as signals
  indicators = cbind(kb, dc)
  names(indicators) = c("kb_low", "kb_high", "dc_low", "dc_high")

  # join to close
  d = cbind(Op(df), Hi(df), Lo(df), Cl(df), indicators)
  names(d)[1:4] = c("open", "high", "low", "close")

  d
}


#'@title compute breadth indicator
#'
#'@param data List of price data
#'@param tickers Character vector of tickers
#'@param threshold Percent difference between the EMA-10 and EMA-50 to consider a stock is in an uptrend
#'@export
compute_breadth <- function(data, tickers, threshold=0.03){
  data = data[intersect(names(data), tickers)]
  tickers = names(data)

  df_breadth = lapply(tickers, function(ticker){
    d = data[[ticker]]
    if(is.null(d)) return(NA)
    ma_fast = EMA(Ad(d), n=10)
    ma_slow = EMA(Ad(d), n=50)

    d = merge(Ad(d), ma_fast)
    d = merge(d, ma_slow)
    names(d) = c("close", "ma_fast", "ma_slow")
    d$ma_prop = d$ma_fast/d$ma_slow - 1
    d[, "ma_prop"]
  })
  names(df_breadth) = tickers

  df_breadth = df_breadth[!is.na(df_breadth)]
  df_breadth = lapply(df_breadth, function(d) d[,"ma_prop"])

  df = do.call(cbind, df_breadth)
  names(df) = tickers

  df_uptrend = apply(df, 1, function(x) {
    x = x[!is.na(x)]
    n = length(x)
    sum(ifelse(x >= threshold, 1, 0), na.rm=T) / n
  }) %>% as.xts(.) %>% .[complete.cases(.)]
  index(df_uptrend) <- as.Date(index(df_uptrend))

  df_mapct = apply(df, 1, function(x) {
    x = x[!is.na(x)]
    mean(x)
  }) %>% as.xts(.) %>% .[complete.cases(.)]
  index(df_mapct) <- as.Date(index(df_mapct))


  list(df_mapct=df_mapct, df_uptrend=df_uptrend)
}
