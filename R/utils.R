#' @importFrom magrittr %>%
#' @keywords internal
magrittr::'%>%'

#' Convert long dataframe to a wide format
#'
#' @param long a dataframe in long format
#'
#' @return a dataframe
#' @export
data_long_to_wide <- function(long){
  wide <- long %>%
    spread(key = "pair", value = "close") %>%
    fill(-time)
  return(wide)
}

#' Convert a wide dataframe to a long format
#'
#' @param wide a dataframe in wide format
#'
#' @return a dataframe
#' @export
data_wide_to_long <- function(wide){
  long <- wide %>%
    gather(key = "pair", value = "close", -time) %>%
    fill(close)
}


#' Index a timeseries at a given point
#'
#' @param wide a dataframe in wide format
#' @param lookback period to look back
#'
#' @return an indexed time-series
#' @export
index_series <- function(wide, lookback){
  N <- nrow(wide)
  N <- min(N-1,lookback)

  indexed <- wide %>%
    drop_na() %>%
    filter(time >= Sys.Date() - N) %>%
    mutate_if(.predicate = is.numeric, .funs = function(x) x * (100 / x[1]))
  return(indexed)
}

#' Convert an R date to the RFC3339 format used in Nomics API
#'
#' @param date a date in POSIXct format
#'
#' @return an rfc3339 representaton of date
#' @export
#' @importFrom lubridate as_datetime
#'
to_rfc3339 <- function(date){
  format(lubridate::as_datetime(date), "%Y-%m-%dT%H:%M:%SZ")
}

#' Convert an RFC3339 date to an R date
#'
#' @param date an RFC3339 date
#'
#' @return a POSIXct representation of date
#' @export
#'
from_rfc3339 <- function(date){
  as.POSIXct(date, format="%Y-%m-%dT%H:%M:%SZ")
}

#' #' Title
#' #'
#' #' @param x
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' is.NullOb <- function(x){
#'   is.null(x) | all(sapply(x, is.null))
#' }
#'
#' #' Title
#' #'
#' #' @param x
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' rmNullObs <- function(x) {
#'   x <- Filter(Negate(is.NullOb), x)
#'   lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
#' }
