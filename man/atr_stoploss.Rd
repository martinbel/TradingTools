% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stoploss.R
\name{atr_stoploss}
\alias{atr_stoploss}
\title{Define a stop loss based on the ATR:}
\usage{
atr_stoploss(data, ticker, n_days = 40, atr_multiple = 1.5)
}
\arguments{
\item{data}{List of price data xts objects}

\item{ticker}{Ticker}

\item{n_days}{Number of periods for the moving average}

\item{atr_multiple}{times I multiply the ATR to substract it from the current close price}
}
\description{
I use the following definition, with default parameters:
   Stop = Close - ATR(40) * multiple
}
