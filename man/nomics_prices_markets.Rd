% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_nomics.R
\name{nomics_prices_markets}
\alias{nomics_prices_markets}
\title{Market prices}
\usage{
nomics_prices_markets(currency = "btc")
}
\arguments{
\item{currency}{(optional) Nomics Currency ID of the desired base currency. Default = 'BTC'.}
}
\value{
A list of markets with their price
}
\description{
\code{nomics_prices_markets} returns prices in USD for the last trade in each market
  with the given base currency.
}
\references{
http://docs.nomics.com/#operation/getMarketPrices
}
