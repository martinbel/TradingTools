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

#' @title Gets fundamental data
#'
#' @param ticker Stock Ticker
#' @param element Element
#' @export
get_fundamentals <- function(ticker, element){
  s = paste0(letters[c(26, 1, 3, 11, 19)], collapse="")
  url = sprintf('https://widget3.%s.com/data/chart/json/%s/%s/www.%s.com', s, ticker, element, s)
  r = GET(url)
  lst = content(r, "parsed")
  nms = names(lst)

  if(element == 'price_and_eps_surprise'){
    element = 'eps_surprise'
    l = lst[[4]]
    df = suppressWarnings(
      data.table(date=as.Date(names(l), format='%m/%d/%y'),
                    as.numeric(as.vector(l))))
  } else {
    df = suppressWarnings(
      rbindlist(lapply(lst, function(l){
      data.table(date=as.Date(names(l), format='%m/%d/%y'),
                 as.numeric(as.vector(l)))
    })))
  }

  df = df[complete.cases(df)]
  setnames(df, "V2", element)
  df = unique(df)[order(date)]

  df
}

