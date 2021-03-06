% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/screens.R
\name{rs_period}
\alias{rs_period}
\title{Computes RS among other performance metrics for a given period.}
\usage{
rs_period(
  data,
  ticker,
  date_range_rs = "2021-02-15/",
  date_range_longterm = "2015/"
)
}
\arguments{
\item{data}{List of price data}

\item{ticker}{ticker selected}

\item{date_range_rs}{Date range to measure RS}

\item{date_range_longterm}{Date range to measure long term performance metrics}
}
\description{
The idea is to look for stocks that did well in a given period as the general market is in a correction.
}
