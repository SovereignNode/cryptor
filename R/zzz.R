credentials <- new.env()

.onLoad <- function(libname, pkgname) {
  enum_interval_kraken <<- enum_interval_kraken()
}
