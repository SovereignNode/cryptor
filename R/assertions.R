#' @keywords internal
is_interval_kraken <- function(x){
  assertthat::assert_that(is.numeric(x), length(x) == 1)
  x %in% enum_interval_kraken
}

#' @keywords internal
assertthat::on_failure(is_interval_kraken) <- function(call, env){
  paste0(deparse(call$x),
         " not a valid interval for Kraken. Please use any of the following: ",
         paste0(as.vector(unlist(enum_interval_kraken)),collapse = ", "))
}

#' @keywords internal
is_pair_kraken <- function(x){
  assertthat::assert_that(is.character(x), length(x) >= 1)
  pairs <- kraken_assetpairs()
  available <- c(pairs$assetpair,
                 pairs$altname)
  all(x %in% available)
}

#' @keywords internal
assertthat::on_failure(is_pair_kraken) <- function(call, env){
  paste0(deparse(call$x),
         " contains an invalid pair for Kraken.")
}

#' @keywords internal
is_ordertype_kraken <- function(x){
  assertthat::assert_that(is.character(x))
  x %in% enum_ordertype_kraken
}

#' @keywords internal
assertthat::on_failure(is_ordertype_kraken) <- function(call, env){
  paste0(deparse(call$x),
         " not a valid ordertype for Kraken. Please use any of the following: ",
         paste0(as.vector(unlist(enum_ordertype_kraken)),collapse = ", "))
}
