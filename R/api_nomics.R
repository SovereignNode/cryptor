#' Utility function to query Nomics API
#'
#' @param endpoint an endpoint
#' @param args a list of arguments to include in the API call
#' @return parsed content from the API call
#' @keywords internal
#'
query_nomics <- function(endpoint, args = NULL){

  if(!is.null(args)){args <- args[!unlist(lapply(X = args, FUN = is.null))]}

  key <- key_nomics()
  post_data <- paste0("https://api.nomics.com/v1","/", endpoint)
  args_key <- list(key = key)
  args_full <- append(args_key, args)

  url <- paste(post_data, paste(paste(names(args_full), args_full, sep = "="), collapse = "&"), sep = "?")

  res <- httr::content(httr::POST(url = url))
  return(res)
}


# Nomics General ----------------------------------------------------------


#' Nomics currencies
#'
#' @description \code{nomics_currencies} returns all the currencies that Nomics supports.
#' @references http://docs.nomics.com/#operation/getCurrencies
#' @return a tibble
#' @export
#' @examples nomics_currencies()
#'
nomics_currencies <- function(){
  dplyr::bind_rows(query_nomics(endpoint = "currencies"))
}

#' Nomics markets
#'
#' @description \code{nomics_markets} returns information on the exchanges and markets that Nomics supports,
#'     in addition to the Nomics currency identifiers for the base and quote currency.
#' @references http://docs.nomics.com/#operation/getMarkets
#' @param exchange (optional) a string of exchange ID to filter by (e.g. "kraken")
#' @param base (optional) a comma separated vector of base currencies to filter by (e.g. "BTC,BCH" / "ETH")
#' @param quote (optional) a comma separated vector of quote currencies to filter by (e.e. "ETH,ETC" / "BCH")
#' @return a tibble
#' @export
#' @examples
#' # Basic query
#' nomics_markets()
#'
#' # Query only 'BTC' markets
#' nomics_markets(base = c("BTC"))
#'
#' # Query only 'kraken' markets
#' nomics_markets(exchange = "kraken")
#'
#' # Query invalid exchange markets
#' \dontrun{
#' nomics_markets(exchange = 123)}
#'
nomics_markets <- function(exchange = NULL, base = NULL, quote = NULL){

  if(!is.null(base)){base <- toupper(base)}
  if(!is.null(quote)){quote <- toupper(quote)}

  query_nomics(endpoint = "markets",
               args = list(exchange = exchange,
                           base = base,
                           quote = quote)) -> res
  res %>% dplyr::bind_rows(res)
}




# Nomics Prices -----------------------------------------------------------

# Returns the latest price quotes


#' The prices endpoint returns current prices for all currencies. Prices are updated every 10 seconds.
#'
#' @return A list of currencies with their price
#' @export
#' @importFrom dplyr bind_rows
#' @examples
nomics_prices <- function(){
  res <- query_nomics(endpoint = "prices")
  do.call(what = dplyr::bind_rows, args = res)
}


#' Market prices
#'
#' @description \code{nomics_prices_markets} returns prices in USD for the last trade in each market
#'   with the given base currency.
#' @references http://docs.nomics.com/#operation/getMarketPrices
#' @param currency (optional) Nomics Currency ID of the desired base currency. Default = 'BTC'.
#' @return A list of markets with their price
#' @export
#' @importFrom dplyr bind_rows
#' @examples
nomics_prices_markets <- function(currency = "btc"){
  dplyr::bind_rows(query_nomics(endpoint = "markets/prices",
                                args = list(currency = toupper(currency))))
}

#' Exchange market prices
#'
#' @description \code{nomics_prices_exchange_markets} returns prices for the last trade in each market
#' @references http://docs.nomics.com/#operation/getExchangeMarketPrices
#' @param currency (optional) Nomics Currency ID to filter by. If present, only markets with this currency as the base or quote will be returned.
#' @param exchange (optional) Nomics Exchange ID to filter by. If present, only markets on this exchange will be returned
#' @return A list of markets with their prices
#' @export
#' @importFrom dplyr bind_rows
#' @examples
nomics_prices_exchange_markets <- function(currency = "btc", exchange = NULL){
  do.call(what = dplyr::bind_rows,
          args = query_nomics(endpoint = "exchange-markets/prices",
                              args = list(currency = toupper(currency),
                                          exchange = exchange)))
}


#' Currencies sparkline
#'
#' @description \code{nomics_currencies_sparkline} returns prices for all currencies
#'   within a customizable time interval suitable for sparkline charts.
#'
#'   Note the timestamps and prices are built off of OHLCV candles using the close price.
#'   This means that the timestamp represents the start of the candle, and the price is the close price of that candle.
#'   This means the response's final timestamp and price value are always as current as possible,
#'   but also that the price is effectively "off" by one candle.
#'   This endpoint is designed to serve as a convenient way to render sparklines,
#'   if you need exactly aligned times and prices you can use the Aggregated OHLCV Candles endpoint.
#'
#' @references http://docs.nomics.com/#operation/getCurrenciesSparkline
#' @param start (optional) Start time of the interval in POSIXct format. If not provided, yesterday is used.
#' @param end (optional) End time of the interval in POSIXct format. If not provided, the current time is used.
#' @return Currency performance over time for all currencies over the requested time period.
#' @importFrom lubridate days
#' @examples
nomics_currencies_sparkline <- function(start = NULL, end = NULL){

  if(is.null(start)){
    start <- Sys.time() - lubridate::days(1)
  }
  if(is.null(end)){
    end <- Sys.time()
  }
  query_nomics(endpoint = "currencies/sparkline", args = list(start = to_rfc3339(start),
                                                              end = to_rfc3339(end)))
}



# Nomics Candles ----------------------------------------------------------


#' The candles endpoint returns aggregated open, high, low, close, and volume information for Nomics currencies.
#' When asking for candles, a currency is provided as a parameter.
#' Nomics aggregates all markets where the given currency is the base currency
#' and the quote currency is a fiat currency, BTC, or ETH and returns all values in USD.
#' Candles are aggregated across all markets for the base currencies, which necessitates converting to a common quote currency.
#' Nomics converts all markets into USD in order to aggregated candles.
#'
#' @param interval (optional) Time interval of the candle ('1d' or '1h'). Default is '1d'.
#' @param currency (optional) Currency ID. Default is 'btc'
#' @param start (optional) Start time of the interval in POSIXct format. If not provided, starts from first candle.
#' @param end (optional) End time of the interval in POSIXct format. If not provided, the current time is used.
#' @return A list of candles
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @export
#'
nomics_candles_aggr <- function(interval = '1d', currency = 'btc', start = NULL, end = NULL){

  if(!is.null(start)){start <- to_rfc3339(start)}
  if(!is.null(end)){end <- to_rfc3339(end)}

  assertthat::assert_that(is_interval_nomics(interval))

  res <- query_nomics(endpoint = "candles",
                      args = list(interval = interval,
                                  currency = toupper(currency),
                                  start = start,
                                  end = end))
  df <- do.call(what = dplyr::bind_rows, args = res)
  df %>%
    dplyr::mutate(timestamp = from_rfc3339(timestamp)) %>%
    dplyr::mutate_if(.predicate = is.character, .funs = as.numeric)
}


#' Exchange candles endpoint
#'
#' @description returns raw open, close, high, low, and volume information for Nomics Markets.
#'   The data is not aggregated, therefore prices are in the quote currency of the market and
#'   volume is in the base currency of the market.
#'
#' @param interval (optional) Time interval of the candle ('1d' or '1h'). Default is '1d'.
#' @param exchange Exchange ID
#' @param market Market ID
#' @param start (optional) Start time of the interval in POSIXct format. If not provided, starts from first candle.
#' @param end (optional) End time of the interval in POSIXct format. If not provided, the current time is used.
#' @return A list of candles
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @export
#' @examples
nomics_candles <- function(interval = '1d', exchange, market, start = NULL, end = NULL){

  if(!is.null(start)){start <- to_rfc3339(start)}
  if(!is.null(end)){end <- to_rfc3339(end)}

  assertthat::assert_that(is_interval_nomics2(interval))

  res <- query_nomics(endpoint = "exchange_candles",
                      args = list(interval = interval,
                                  exchange = exchange,
                                  market = toupper(market),
                                  start = start,
                                  end = end))

  df <- do.call(what = dplyr::bind_rows, args = res)
  df %>%
    dplyr::mutate(timestamp = from_rfc3339(timestamp)) %>%
    dplyr::mutate_if(.predicate = is.character, .funs = as.numeric)
}


# Nomics Market Caps -----------------------------------------------------------



#' Market capitalization for individuals currencies
#'
#' @description \code{nomics_mcap_indiv} returns the individual market caps for for all cryptoassets
#'   at a given date
#'
#' @param date (required) Date to return market caps for in POSIXct format.
#'
#' @return Market capitalizations for all currencies on a given date.
#' @export
#'
#' @examples
nomics_mcap_indiv <- function(date){

  start <- date - lubridate::days(1)
  out <- tryCatch(
    {
      nomics_currencies() %>%
        dplyr::left_join(y = (nomics_interval_currencies(start = start, end = date) %>%
                                dplyr::select(currency, close_timestamp, close)), by = c('id' = 'currency')) %>%
        dplyr::left_join(y = (nomics_interval_supplies(start = start, end = date) %>%
                                dplyr::select(currency, close_timestamp, close_available, close_max)), by = c('id' = 'currency')) %>%
        dplyr::mutate(close_mcap = close * close_available) %>%
        dplyr::arrange(desc(close_mcap)) %>%
        dplyr::rename(close_timestamp = close_timestamp.x) %>%
        dplyr::select(id, close_timestamp, close_mcap)
    },
    error=function(cond) {
      # Choose a return value in case of error
      return(NULL)
    },
    warning=function(cond) {
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    }
  )
  return(out)
}





# Nomics Volume -----------------------------------------------------------


#' Volume History is the total volume for all cryptoassets in USD at intervals between the requested time period.
#'
#' @param start (required) Start time of the interval in POSIXct format.
#' @param end (optional) End time of the interval in POSIXct format. If not provided, the current time is used.
#' @return Volume over time for all currencies
#' @export
#' @examples
nomics_volume_global <- function(start = NULL, end = NULL){

  if(is.null(start)){
    stop("start input argument is required.")
  }
  res <- query_nomics(endpoint = "volume/history", args = list(start = to_rfc3339(start),
                                                               end = to_rfc3339(end)))
  res <- dplyr::bind_rows(res) %>%
    dplyr::mutate(timestamp = from_rfc3339(timestamp)) %>%
    dplyr::mutate(volume = as.numeric(volume))
  res
}



# Nomics FX ---------------------------------------------------------------


#' The exchange rates endpoint returns the current exchange rates used by Nomics to convert prices from markets into USD.
#' This contains Fiat currencies as well as a BTC and ETH quote prices.
#' This endpoint helps normalize data across markets as well as to provide localization for users.
#'
#' @return List of exchange rates into USD
#' @export
#' @examples
nomics_exchange_rates <- function(){
  res <- query_nomics(endpoint = "exchange-rates")
  res <- dplyr::bind_rows(res)
  res
}


#' Exchange rates for every point in a time range.
#' This endpoint can be used with other interval endpoints to convert values into a desired quote currency.
#' The currency parameter must be a Nomics Quote Currency, to get all Nomics Quote Currencies,
#' use the /exchange-rates endpoint for all current rates
#'
#' @param currency (require) Currency ID
#' @param start (required) Start time of the interval in POSIXct format.
#' @param end (optional) End time of the interval in POSIXct format. If not provided, the current time is used.
#' @return Exchange rates for every point in a time range
#' @export
#' @examples
nomics_exchange_rates_history <- function(currency, start, end = NULL){

  if(!is.null(start)){
    start <- to_rfc3339(start)
  }else{
    stop("start input argument is required.")
  }
  if(!is.null(end)){end <- to_rfc3339(end)}

  res <- query_nomics(endpoint = "exchange-rates/interval", args = list(currency = currency, start = start, end = end))
  res <- dplyr::bind_rows(res) %>%
    dplyr::mutate(open_timestamp = from_rfc3339(open_timestamp),
                  close_timestamp = from_rfc3339(close_timestamp)) %>%
    dplyr::mutate(open = as.numeric(open),
                  close = as.numeric(close))
  res
}


# Nomics Intervals --------------------------------------------------------

#' The market interval endpoint returns a summary of information about all markets (in USD) based
#' in a given currency over a configurable time interval.
#'
#' @param currency (required) Nomics currency id to query information for
#' @param hours (optional) Number of hours back to calculate date [1 ... 87600]. Default = 1
#' @param start (optional) Start time of the interval (as POSIXct). If not provided, it is computed using the hours parameter.
#' @param end (optional) End time of the interval (as POSIXct). If not provided, the current time is used.
#' @return A list of markets with price and volume information for a currency.
#' @export
#' @importFrom dplyr bind_rows
#' @examples
nomics_interval_markets <- function(currency, hours = NULL, start = NULL, end = NULL){
  if(!is.null(start)){start <- to_rfc3339(start)}
  if(!is.null(end)){end <- to_rfc3339(end)}
  if(is.null(start) & is.null(hours)){stop("Function requires atleast one of 'hours' or 'start' as input")}

  res <- query_nomics(endpoint = "markets/interval",
                      args = list(currency = currency, hours = hours, start = start, end = end))
  dplyr::bind_rows(res)
}

#' The exchange market interval endpoint returns a summary of information
#' about all markets over a configurable time interval in their native values.
#'
#' @param currency (optional) Nomics Currency ID to filter by. If present, only markets with this currency as the base or quote will be returned.
#' @param exchange (optional) Nomics Exchange ID to filter by. If present, only markets on this exchange will be returned
#' @param start (required) Start time of the interval (as POSIXct).
#' @param end (optional) End time of the interval (as POSIXct). If not provided, the current time is used.
#' @return A list of markets with price and volume information for a currency
#' @export
#' @importFrom dplyr bind_rows
#' @examples
nomics_interval_exchange_markets <- function(currency = NULL, exchange = NULL, start, end = NULL){

  if(!is.null(start)){
    start <- to_rfc3339(start)
  }else{
    stop("start input argument is required.")
  }
  if(!is.null(end)){end <- to_rfc3339(end)}

  res <- query_nomics(endpoint = "exchange-markets/interval",
                      args = list(currency = currency, exchange = exchange, start = start, end = end))
  df     <- do.call(what = dplyr::bind_rows,
                    args = plyr::llply(.data = res,
                                       .fun = function(x) x[unlist(lapply(X = x,
                                                                          FUN = function(x) length(x) == 1))]
                    )
  )
  df
}

#' Exchange rates to convert from USD over a time interval.
#' This endpoint can be used with other interval endpoints to convert values into a desired quote currency.
#'
#' @param start (required) Start time of the interval in POSIXct format.
#' @param end (optional) End time of the interval in POSIXct format. If not provided, the current time is used.
#' @return List of exchange rates open and close information
#' @export
#' @examples
nomics_interval_exchange_rates <- function(start, end = NULL){

  if(!is.null(start)){
    start <- to_rfc3339(start)
  }else{
    stop("start input argument is required.")
  }
  if(!is.null(end)){end <- to_rfc3339(end)}

  res <- query_nomics(endpoint = "exchange-rates/interval", args = list(start = start, end = end))
  res <- dplyr::bind_rows(res) %>%
    dplyr::mutate(open_timestamp = from_rfc3339(open_timestamp),
                  close_timestamp = from_rfc3339(close_timestamp)) %>%
    dplyr::mutate(open = as.numeric(open),
                  close = as.numeric(close))
  res
}

#' Open and close prices and volume for all currencies between a customizable time range.
#'
#' @param start (required) Start time of the interval in POSIXct format.
#' @param end (optional) End time of the interval in POSIXct format. If not provided, the current time is used.
#' @return Open, close, and volume for all currencies in a time range
#' @export
#' @examples
nomics_interval_currencies <- function(start, end = NULL){

  if(!is.null(start)){
    start <- to_rfc3339(start)
  }else{
    stop("start input argument is required.")
  }
  if(!is.null(end)){end <- to_rfc3339(end)}

  res <- query_nomics(endpoint = "currencies/interval", args = list(start = start, end = end))
  res <- dplyr::bind_rows((x = res))
  if(plyr::empty(res)){warning("Empty frame returned");return(res)}
  res <- res %>%
    dplyr::mutate(open_timestamp = from_rfc3339(open_timestamp),
                  close_timestamp = from_rfc3339(close_timestamp)) %>%
    dplyr::mutate(open = as.numeric(open),
                  close = as.numeric(close),
                  volume = as.numeric(volume))
  res
}


#' Open and close supply information for all currencies between a customizable time interval.
#'
#' @param start (required) Start time of the interval in POSIXct format.
#' @param end (optional) End time of the interval in POSIXct format. If not provided, the current time is used.
#' @return Open and close supply information
#' @export
#' @examples
nomics_interval_supplies <- function(start, end = NULL){

  if(!is.null(start)){
    start <- to_rfc3339(start)
  }else{
    stop("start input argument is required.")
  }
  if(!is.null(end)){end <- to_rfc3339(end)}

  res <- query_nomics(endpoint = "supplies/interval", args = list(start = start, end = end))
  df     <- do.call(what = dplyr::bind_rows,
                    args = plyr::llply(.data = res,
                                       .fun = function(x) x[unlist(lapply(X = x,
                                                                          FUN = function(x) length(x) == 1))]
                    )
  )

  if(plyr::empty(df)){warning("Empty frame returned");return(df)}

  df %>%
    dplyr::mutate(open_timestamp = from_rfc3339(open_timestamp),
                  close_timestamp = from_rfc3339(close_timestamp)) %>%
    dplyr::mutate(open_available = as.numeric(open_available),
                  open_max = as.numeric(open_max),
                  close_available = as.numeric(close_available),
                  close_max = as.numeric(close_max))
}


#' Market capitalization for individuals currencies historical
#'
#' @description \code{nomics_interval_mcap_indiv} returns the individual market caps for all cryptoassets
#'   between a start and end date.
#'
#' @param start (optional) Start time of the interval in POSIXct format. If not provided, yesterday will be used.
#' @param end (optional) End time of the interval in POSIXct format. If not provided, the current time is used.
#'
#' @return
#' @export
#'
#' @examples
nomics_interval_mcap_indiv <- function(start = NULL, end = NULL){
  if(is.null(start)){
    start <- Sys.time() - lubridate::days(1)
  }
  if(is.null(end)){
    end <- Sys.time()
  }
  dates <- seq.POSIXt(from = start, to = end, by = 'day')
  dplyr::bind_rows(lapply(X = dates, FUN = nomics_mcap_indiv))
}

#' Market capitalization history in aggregate
#'
#' @description \code{nomics_interval_mcap_aggr} returns the aggregate market cap for all cryptoassets
#'   at intervals between the requested time period.
#' @param start (optional) Start time of the interval in POSIXct format. If not provided, yesterday will be used.
#' @param end (optional) End time of the interval in POSIXct format. If not provided, the current time is used.
#' @return Performance over time for the entire market.
#' @export
#' @examples
nomics_interval_mcap_aggr <- function(start = NULL, end = NULL){

  if(is.null(start)){
    start <- Sys.time() - lubridate::days(1)
  }
  if(is.null(end)){
    end <- Sys.time()
  }
  query_nomics(endpoint = "market-cap/history",
               args = list(start = to_rfc3339(start),
                           end = to_rfc3339(end))) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(timestamp = from_rfc3339(timestamp)) %>%
    dplyr::mutate(market_cap = as.numeric(market_cap))
}
