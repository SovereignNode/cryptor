
is_interval_kraken <- function(x){
  assertthat::assert_that(is.numeric(x), length(x) == 1)
  x %in% enum_interval_kraken
}

assertthat::on_failure(is_interval_kraken) <- function(call, env){
  paste0(deparse(call$x),
         " not a valid interval for Kraken. Please use any of the following: ",
         paste0(as.vector(unlist(enum_interval_kraken)),collapse = ", "))
}


is_pair_kraken <- function(x){
  assertthat::assert_that(is.character(x), length(x) >= 1)
  pairs <- kraken_assetpairs()
  available <- c(pairs$assetpair,
                 pairs$altname)
  all(x %in% available)
}

assertthat::on_failure(is_pair_kraken) <- function(call, env){
  paste0(deparse(call$x),
         " not a valid pair for Kraken.")
}
