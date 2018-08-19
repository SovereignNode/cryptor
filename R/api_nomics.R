query_nomics <- function(endpoint, args = list()){

  key <- key_nomics()

  post_data <- paste0("https://api.nomics.com/v1","/",endpoint)

  args_key <- list(key = key)
  args_full <- append(args_key, args)

  url <- paste(post_data, paste(paste(names(args_full), args_full, sep = "="), collapse = "&"), sep = "?")

  res <- httr::POST(url = url)
  res <- httr::content(res)

  return(res)

}


# nomics_markets <- function(){
#   res <- query_nomics(endpoint = "markets", args = list())
#   dplyr::bind_rows(res)
# }

# nomics_markets_interval <- function(currency){
#   query_nomics(endpoint = "markets/interval", args = list(currency = currency))
# }
#
# nomics_markets_prices <- function(currency){
#   query_nomics(endpoint = "markets/prices", args = list(currency = currency))
# }
#
# nomics_exchange_interval <- function(){
#   query_nomics(endpoint = "exchange-markets/interval", args = list())
# }


#' #' Title
#' #'
#' #' @param start
#' #' @param end
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' nomics_currencies_interval <- function(start, end){
#'   start <- format(lubridate::as_datetime(start), "%Y-%m-%dT%H:%M:%SZ")
#'   end <- format(lubridate::as_datetime(end), "%Y-%m-%dT%H:%M:%SZ")
#'   res <- query_nomics(endpoint = "currencies/interval", args = list(start = start, end = end))
#'   dplyr::bind_rows((x = res))
#' }
#'
#' #' Title
#' #'
#' #' @param start
#' #' @param end
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' nomics_supplies_interval <- function(start, end){
#'   start <- format(lubridate::as_datetime(start), "%Y-%m-%dT%H:%M:%SZ")
#'   end <- format(lubridate::as_datetime(end), "%Y-%m-%dT%H:%M:%SZ")
#'   res <- query_nomics(endpoint = "supplies/interval", args = list(start = start, end = end))
#'   dplyr::bind_rows((x = res))
#' }
#'
#' #' Title
#' #'
#' #' @param start
#' #' @param end
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' nomics_marketcap <- function(start, end){
#'   start <- format(lubridate::as_datetime(start), "%Y-%m-%dT%H:%M:%SZ")
#'   end <- format(lubridate::as_datetime(end), "%Y-%m-%dT%H:%M:%SZ")
#'   query_nomics(endpoint = "market-cap/history", args = list(start = start, end = end))
#' }
