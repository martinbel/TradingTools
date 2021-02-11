#' @title Convert an xts object to data.table
#'
#' @param df_xts xts object with stock data
#' @export
xts_to_dt <- function(df_xts){
  df_xts = as.data.table(df_xts)
  ticker = gsub("\\.Open", "", names(df_xts))[2]
  setnames(df_xts, "index", "date")
  setnames(df_xts, names(df_xts)[-1], tolower(gsub(paste0(ticker, "\\."), "", names(df_xts)[-1])))
  cbind(ticker, df_xts)
}

#' @title Get first date of each ticker
#'
#' @param data xts object with stock data
#' @export
get_start_dates <- function(data){
  close_adj = lapply(data, Ad)
  start_dates = rbindlist(lapply(names(close_adj), function(tk){
    x = head(close_adj[[tk]], 1)
    data.table(ticker=tk, start_date=as.Date(index(x)))
  }))
  start_dates[order(start_date)]
}


#'@title  Find which col to adjust
#'
#'@param x xts price data
#'@param str String to look for
#'@export
which_col <- function(x, str='Adjusted') {
  grep(str, colnames(x), ignore.case = TRUE)
}


#' @title Reads fundamental data and corrects some errors
#'
#' @param fund_data_file File
#' @param ticker_lkp Tickers look up table
#' @export
read_fund_data <- function(fund_data_file, ticker_lkp){
  df = readRDS(fund_data_file)
  df = rbindlist(df[sapply(df, function(x) class(x)[1]) == 'data.table'])

  for(j in c("market_cap", "enterprise_value")){
    df[, (j):=ifelse(grepl(" Til", get(j)), as.numeric(gsub(" Til", "", get(j))) * 1000*1000,
                     ifelse(grepl(" Bil", get(j)), as.numeric(gsub(" Bil", "", get(j)))*1000,
                            as.numeric(gsub(" Mil", "", get(j)))))]
  }

  df = df[, unique(names(df)), with=F]
  df = merge(ticker_lkp[, .(ticker, name, sector, exchange, index)], df, by='ticker')
  df[, score:=rowMeans(df[, .(financial_strength, profitability_rank, valuation_rank)], na.rm=T)]
  df[, index:=NULL]
  df = unique(df)

  df
}
