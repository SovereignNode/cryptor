
# Utils -------------------------------------------------------------------


credentials <- new.env()



# Binance -----------------------------------------------------------------


#' Look up Binance API secret stored in the environment
#' @return string
#' @keywords internal
secret_binance <- function() {
  check_credentials_binance()
  credentials$secret_binance
}


#' Look up Binance API key stored in the environment
#' @return string
#' @keywords internal
key_binance <- function() {
  check_credentials_binance()
  credentials$key_binance
}


#' Sets the API key and secret to interact with the Binance API
#' @param key string
#' @param secret string
#' @export
#' @examples \dontrun{
#' credentials_binance('foo', 'bar')
#' }
credentials_binance <- function(key, secret) {
  credentials$key_binance <- key
  credentials$secret_binance <- secret
}


#' Check if Binance credentials were set previously
#' @return fail on missing credentials
#' @keywords internal
check_credentials_binance <- function() {
  if (is.null(credentials$secret_binance)) {
    stop('Binance API secret not set? Call credentials_binance()')
  }
  if (is.null(credentials$key_binance)) {
    stop('Binance API key not set? Call credentials_binance()')
  }
}




# Kraken ------------------------------------------------------------------




#' Look up Kraken API secret stored in the environment
#' @return string
#' @keywords internal
secret_kraken <- function() {
  check_credentials_kraken()
  credentials$secret_kraken
}


#' Look up Kraken API key stored in the environment
#' @return string
#' @keywords internal
key_kraken <- function() {
  check_credentials_kraken()
  credentials$key_kraken
}


#' Sets the API key and secret to interact with the Kraken API
#' @param key string
#' @param secret string
#' @export
#' @examples \dontrun{
#' credentials_kraken('foo', 'bar')
#' }
credentials_kraken <- function(key, secret) {
  credentials$key_kraken <- key
  credentials$secret_kraken <- secret
}


#' Check if Kraken credentials were set previously
#' @return fail on missing credentials
#' @keywords internal
check_credentials_kraken <- function() {
  if (is.null(credentials$secret_kraken)) {
    stop('Kraken API secret not set? Call credentials_kraken()')
  }
  if (is.null(credentials$key_kraken)) {
    stop('Kraken API key not set? Call credentials_kraken()')
  }
}



# Nomics ------------------------------------------------------------------

#' Look up Nomics API key stored in the environment
#' @return string
#' @keywords internal
key_nomics <- function() {
  check_credentials_nomics()
  credentials$key_nomics
}


#' Sets the API key and secret to interact with the Nomics API
#' @param key string
#' @export
#' @examples \dontrun{
#' credentials_nomics('foo')
#' }
credentials_nomics <- function(key) {
  credentials$key_nomics <- key
}

#' Check if Nomics credentials were set previously
#' @return fail on missing credentials
#' @keywords internal
check_credentials_nomics <- function() {
  if (is.null(credentials$key_nomics)) {
    stop('Nomics API key not set? Call credentials_nomics()')
  }
}

