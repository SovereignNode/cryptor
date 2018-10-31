.onLoad <- function(libname, pkgname) {
  enum_interval_kraken <<- enum_interval_kraken()
  enum_ordertype_kraken <<- enum_ordertype_kraken()
  enum_interval_nomics <<- enum_interval_nomics()
  enum_interval_nomics2 <<- enum_interval_nomics2()
}
