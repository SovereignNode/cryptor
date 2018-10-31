enum_interval_kraken <- function(){
  list('1m' = 1,
       '5m' = 5,
       '15m' = 15,
       '30m' = 30,
       '1h' = 60,
       '4h' = 240,
       '1d' = 1440,
       '1w' = 10080,
       '1M' = 21600)
}

enum_ordertype_kraken <- function(){
  list('market' = 'market',
       'limit' = 'limit')
}

enum_interval_nomics <- function(){
  list('1h' = '1h',
       '1d' = '1d')
}

enum_interval_nomics2 <- function(){
  list('1m' = '1m',
       '5m' = '5m',
       '30m' = '30m',
       '1h' = '1h',
       '4h' = '4h',
       '1d' = '1d')
}
