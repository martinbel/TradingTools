#' @title Downloads price data
#'
#' @param tickers Character vector of tickers
#' @param from Starting date
#' @param to end date, defaults to the current date if it's NULL.
#' @export
get_price_data <- function(tickers, from='1990-01-01', to=NULL){
  if(is.null(to)){
    to = Sys.Date()
  }

  data = list()
  for(tk in tickers){
    data[[tk]] = getSymbols(tk, from=from, to=to, auto.assign=FALSE)
  }
  data
}

#' @title Compute returns of a list of price data
#'
#' @param data List of stocks price data
#' @export
list_to_returns <- function(data){
  close_adj = lapply(data, Ad)
  R = lapply(close_adj, ROC, n=1, type = "discrete")
  ret = do.call(merge, R)
  ret = ret[-1, ]
  names(ret) = gsub("\\.Adjusted", "", names(ret))

  ret
}
