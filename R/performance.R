#' @title Most common performance metrics for each individual ticker
#'
#' @param data Daily price data
#' @param ticker Selected ticker
#' @param date_range A string with an xts style date-range to filter data
#' @param index Benchmark index to compute CAPM Beta and Alpha
#' @export
performance_ticker <- function(data, ticker, date_range=NULL, index='SPY'){

  if(is.null(date_range)){
    date_range='/'
  }

  # keep complete cases
  close = Ad(data[[ticker]])
  ret_index = ROC(Ad(data[[index]]), type='discrete')
  ret_index = ret_index[complete.cases(ret_index)]

  ret = ROC(close[date_range], n=1, type='discrete')
  ret = ret[complete.cases(ret)]

  # CAPM
  R_index = apply.monthly(ret_index, Return.cumulative)
  Ra = apply.monthly(ret, Return.cumulative)
  df_capm = merge(Ra, R_index)
  df_capm = df_capm[complete.cases(df_capm)]

  fit = lm(df_capm[,1] ~ df_capm[,2])
  coefs = coef(fit)

  # Long term performance
  max_dd = maxDrawdown(ret)
  cum_return = Return.cumulative(ret)[[1]]
  tbl = table.AnnualizedReturns(ret)[,1]

  # Summary table
  tbl = data.table(ticker, CAGR=tbl[1], Volatility=tbl[2], MaxDrawdown=max_dd,
                   Sharpe=tbl[3], MAR=tbl[1]/max_dd,
                   TotalReturn=cum_return,
                   Beta=coefs[[2]], Alpha=coefs[[1]])
  tbl
}


#' @title Most common performance metrics
#'
#' @param ret_mat The daily returns matrix
#' @param date_range A string with an xts style date-range to filter data
#' @param tickers Character vector with tickers that will be part of the result
#' @param combine Character vector of tickers to combine in an equal weight portfolio. Results are summarized as Portfolio
#' @param index Benchmark index to compute CAPM Beta and Alpha
#' @param plot Plot the cumulative return of all assets
#' @param print_cor Print the correlation matrix of the assets defined in ticker
#' @param print_tbl Prints the summary performance table
#' @export
performance <- function(ret_mat,
                        date_range=NULL,
                        tickers=NULL,
                        combine=NULL,
                        index="QQQ",
                        plot=TRUE,
                        print_cor=FALSE,
                        print_tbl=FALSE){

  if(is.null(tickers)){
    tickers = names(ret_mat)
  }

  ret_mat = ret_mat[, tickers]

  if(!is.null(date_range)){
    ret_mat = ret_mat[date_range,]
  } else {
    date_range = ''
  }

  # keep complete cases
  ret_mat = ret_mat[complete.cases(ret_mat)]

  if(!is.null(combine)){
    portfolio_return = rowMeans(ret_mat[, combine])
    ret_mat$Portfolio = portfolio_return
    tickers = c(tickers, "Portfolio")
  }

  # compute betas
  res_ticker = names(ret_mat)
  capm = do.call(rbind, lapply(res_ticker, function(j){
    R_index = apply.monthly(ret_mat[,index], Return.cumulative)
    Ra = apply.monthly(ret_mat[,j], Return.cumulative)
    fit = lm(Ra ~ R_index)
    coefs = coef(fit)
    data.table(ticker=j,
               Beta=coefs[[2]],
               Alpha=coefs[[1]])
  }))
  capm = t(capm[, -1, wi=F])
  colnames(capm) = res_ticker


  # compute performance

  # Max-drawdown
  max_dd = maxDrawdown(ret_mat)
  rownames(max_dd) = 'Max Drawdown'

  # Total Return
  cum_return = Return.cumulative(ret_mat)
  rownames(cum_return) = "Total Return"

  # Basic Metrics
  tbl = table.AnnualizedReturns(ret_mat)
  rownames(tbl) = c("CAGR", "Volatility", "Sharpe")

  tbl = rbind(tbl[1,], max_dd, tbl[2,], tbl[3,],
              MAR=tbl[1,] / max_dd,
              cum_return)
  tbl = rbind(tbl, capm)

  if(plot){
    charts.PerformanceSummary(ret_mat, geometric=TRUE,
                              wealth.index=FALSE)
  }

  if(print_cor){
    cat("Correlation matrix\n")
    print(cor(ret_mat))
    cat("\n")
  }

  if(print_tbl){
    cat("Performance Metrics\n")
    print(round(tbl,7))
    cat("\n")
  }

  list(tbl=round(tbl,7), ret_mat=ret_mat, cor_mat=cor(ret_mat))
}


#' @title Compute Calendar returns at the monthly and yearly level.
#'
#' @param rets Daily Return xts object for the asset
#' @param digits Number of digits
#' @param percent Use Percents or decimals?
#' @export
calendar_returns <- function(rets, digits = 3, percent = FALSE) {

  pastePerc <- function(x) paste0(scales::comma(x),"%")
  rowGsub <- function(x) gsub("NA%", "NA", x)

  # get maximum drawdown using daily returns
  dds <- apply.yearly(rets, maxDrawdown)

  # get monthly returns
  rets <- apply.monthly(rets, Return.cumulative)

  # convert to data frame with year, month, and monthly return value
  dfRets <- cbind(year(index(rets)), month(index(rets)), coredata(rets))

  # convert to data table and reshape into year x month table
  dfRets <- data.frame(dfRets)
  colnames(dfRets) <- c("Year", "Month", "Value")
  monthNames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  for(i in 1:length(monthNames)) {
    dfRets$Month[dfRets$Month==i] <- monthNames[i]
  }
  dfRets <- data.table(dfRets)
  dfRets <- data.table::dcast(dfRets, Year~Month)

  # create row names and rearrange table in month order
  dfRets <- data.frame(dfRets)
  yearNames <- dfRets$Year
  rownames(dfRets) <- yearNames; dfRets$Year <- NULL
  dfRets <- dfRets[,monthNames]

  # append yearly returns and drawdowns
  yearlyRets <- apply.yearly(rets, Return.cumulative)
  dfRets$Annual <- yearlyRets
  dfRets$DD <- dds

  # convert to percentage
  if(percent) {
    dfRets <- dfRets * 100
  }

  # round for formatting
  dfRets <- apply(dfRets, 2, round, digits)

  # paste the percentage sign
  if(percent) {
    dfRets <- apply(dfRets, 2, pastePerc)
    dfRets <- apply(dfRets, 2, rowGsub)
    dfRets <- data.frame(dfRets)
    rownames(dfRets) <- yearNames
  }

  as.data.table(dfRets)
}
