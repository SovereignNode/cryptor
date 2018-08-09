key <- "9bd54db51ed9bf555dad7a2a457294ff"
url_base <- "https://api.nomics.com/v1"
endpoint <- "markets"

post_data <- paste0(url_base,"/",endpoint)
args <- list(key = key)

url <- paste(post_data, paste(paste(names(args), args, sep = "="), collapse = "&"), sep = "?")

res <- httr::POST(url = url)
markets <- bind_rows(content(res))



endpoint <- "markets/interval"

post_data <- paste0(url_base,"/",endpoint)
args <- list(key = key, currency = "BTC")

url <- paste(post_data, paste(paste(names(args), args, sep = "="), collapse = "&"), sep = "?")

res <- httr::POST(url = url)
bind_rows(content(res))


endpoint <- "markets/prices"

post_data <- paste0(url_base,"/",endpoint)
args <- list(key = key, currency = "BTC")

url <- paste(post_data, paste(paste(names(args), args, sep = "="), collapse = "&"), sep = "?")

res <- httr::POST(url = url)
bind_rows(content(res))


endpoint <- "exchange-markets/interval"

post_data <- paste0(url_base,"/",endpoint)
args <- list(key = key)

url <- paste(post_data, paste(paste(names(args), args, sep = "="), collapse = "&"), sep = "?")

res <- httr::POST(url = url)
bind_rows(content(res))



endpoint <- "exchange-markets/prices"

post_data <- paste0(url_base,"/",endpoint)
args <- list(key = key)

url <- paste(post_data, paste(paste(names(args), args, sep = "="), collapse = "&"), sep = "?")

res <- httr::POST(url = url)
bind_rows(content(res))




endpoint <- "prices"

post_data <- paste0(url_base,"/",endpoint)
args <- list(key = key)

url <- paste(post_data, paste(paste(names(args), args, sep = "="), collapse = "&"), sep = "?")

res <- httr::POST(url = url)
bind_rows(content(res))



endpoint <- "candles"

post_data <- paste0(url_base,"/",endpoint)
args <- list(key = key, interval = "1d", currency = "BTC")

url <- paste(post_data, paste(paste(names(args), args, sep = "="), collapse = "&"), sep = "?")

res <- httr::POST(url = url)
bind_rows(content(res))



endpoint <- "exchange_candles"

post_data <- paste0(url_base,"/",endpoint)
args <- list(key = key, interval = "1m", exchange = "bitfinex", market = "avtbtc")

url <- paste(post_data, paste(paste(names(args), args, sep = "="), collapse = "&"), sep = "?")

res <- httr::POST(url = url)
bind_rows(content(res))



endpoint <- "dashboard"

post_data <- paste0(url_base,"/",endpoint)
args <- list(key = key)

url <- paste(post_data, paste(paste(names(args), args, sep = "="), collapse = "&"), sep = "?")

res <- httr::POST(url = url)
bind_rows(content(res))



endpoint <- "market-cap/history"

post_data <- paste0(url_base,"/",endpoint)
args <- list(key = key, start = "2017-07-14T00%3A00%3A00Z")

url <- paste(post_data, paste(paste(names(args), args, sep = "="), collapse = "&"), sep = "?")

res <- httr::POST(url = url)
bind_rows(content(res))




endpoint <- "exchange-rates"

post_data <- paste0(url_base,"/",endpoint)
args <- list(key = key)

url <- paste(post_data, paste(paste(names(args), args, sep = "="), collapse = "&"), sep = "?")

res <- httr::POST(url = url)
bind_rows(content(res))
