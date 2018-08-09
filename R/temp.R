rm(list=ls())

key <- "pXWTzAIt2vAUq+L+mkabpYHbYHy/iCd1qZ9+njR2plKEBUAfW+9VATjs"
secret <- "NlQSADg8lm7p20ef/qGvuL1WggM7g5YnZ7ehEX/AK6zS88wvQENjx+L5LSh2rTglZ3MvUJgTXLArG4mA/V/XGw=="

library(cryptor)
library(tidyverse)

credentials_kraken(key = key, secret = secret)



# pair = "BCHEUR"
# type = "buy"
# ordertype = "limit"
# price = "100"
# price2 = "90"
# volume = "0.2"


# kraken_add_order(pair = pair, type = type, ordertype = ordertype, price = price, price2 = price2, volume = volume, validate = TRUE)



# Kraken Examples ----------------------------------------------------------------

assetpairs <- kraken_assetpairs()

# historical allocation
ledger <- kraken_ledger()



# current allocation
allocation <- read.csv2(file = "C:/Crypto/0_Accounting/Allocation.csv",stringsAsFactors = FALSE)

balance <- kraken_balance()
balance <- balance %>% left_join(y = assetpairs %>% filter(quote == "ZEUR") %>% select(base, altname), by = c("asset" = "base"))
balance <- balance %>% left_join(y = allocation, by = "asset")

kraken_lastprice <- function(pair){
  as.numeric(kraken_ticker(pair = pair)$result[[1]]$c[[1]])
}


pairs <- balance  %>% filter(altname != "NA") %>% pull(altname)
prices <- sapply(X = pairs, kraken_lastprice)
prices <- as_tibble(prices) %>% mutate(asset = names(prices))

balance %>% left_join(y = prices,by = c("altname" = "asset")) %>% mutate(weight = if_else(is.na(weight),0,weight),
                                                                         value_quote = balance * value,
                                                                         value = if_else(asset == "ZEUR", 1, value),
                                                                         assets = sum(value_quote,na.rm = TRUE),
                                                                         weight_current = value_quote / assets,
                                                                         value_quote_target = weight * assets,
                                                                         value_quote_diff = value_quote_target - value_quote,
                                                                         balance_target = value_quote_target / value,
                                                                         balance_diff = balance_target - balance) -> balance


# prices
