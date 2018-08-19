#' @importFrom magrittr %>%
#' @keywords internal
magrittr::'%>%'

#' Title
#'
#' @param long
#'
#' @return
#' @export
#'
#' @examples
data_long_to_wide <- function(long){
  wide <- long %>%
    spread(key = "pair", value = "close") %>%
    fill(-time)
  return(wide)
}

#' Title
#'
#' @param wide
#'
#' @return
#' @export
#'
#' @examples
data_wide_to_long <- function(wide){
  long <- wide %>%
    gather(key = "pair", value = "close", -time) %>%
    fill(close)
}


#' Title
#'
#' @param wide
#' @param lookback
#'
#' @return
#' @export
#'
#' @examples
index_series <- function(wide, lookback){
  N <- nrow(wide)
  N <- min(N-1,lookback)

  indexed <- wide %>%
    drop_na() %>%
    filter(time >= Sys.Date() - N) %>%
    mutate_if(.predicate = is.numeric, .funs = function(x) x * (100 / x[1]))
  return(indexed)
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
