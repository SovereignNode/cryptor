credentials_nomics(key = "9bd54db51ed9bf555dad7a2a457294ff")
date_seq <- seq.Date(from = lubridate::ymd(20180101),to = lubridate::ymd(20180810),by = 1)


available_supply <- lapply(X = date_seq, FUN = nomics_supplies_interval, end = lubridate::ymd(20180810))
supply <- bind_rows(available_supply) %>%
  select(currency, open_available, open_timestamp) %>%
  rename(open_available_timestamp = open_timestamp) %>%
  mutate(open_available_date = as.Date(as.POSIXct(open_available_timestamp, format="%Y-%m-%dT%H:%M:%SZ"))) %>%
  select(currency, open_available, open_available_date)

available_opens <- lapply(X = date_seq, FUN = nomics_currencies_interval, end = lubridate::ymd(20180810))
opens <- bind_rows(available_opens) %>%
  select(currency, open, open_timestamp) %>%
  mutate(open_date = as.Date(as.POSIXct(open_timestamp, format="%Y-%m-%dT%H:%M:%SZ"))) %>%
  select(currency, open, open_date)


combined <- left_join(x = supply, y = opens, by = c("currency" = "currency", "open_available_date" = "open_date"))
combined <- combined %>% mutate(open_available = as.numeric(open_available),
                                open = as.numeric(open)) %>%
  select(open_available_date, everything()) %>%
  arrange(open_available_date) %>%
  mutate(mcap = open_available * open)

currencies <- combined %>%
  filter(open_available_date == lubridate::ymd(20180805)) %>%
  filter(mcap != "NA") %>%
  arrange(desc(mcap)) %>%
  top_n(50) %>%
  pull(currency)

combined <- combined %>% filter(currency %in% currencies)

write.csv2(x = combined, file = "C:/Crypto/5_Data/Market_cap_summary.csv")

summed_caps <- bind_rows(nomics_marketcap(start = lubridate::ymd(20180101), end = lubridate::ymd(20180810))) %>%
  mutate(timestamp = as.Date(as.POSIXct(timestamp, format="%Y-%m-%dT%H:%M:%SZ")),
         mcap_total = as.numeric(market_cap))

df <- combined %>%
  left_join(y = summed_caps, by = c("open_available_date" = "timestamp"))


df <- df %>% left_join(y = df %>% filter(currency %in% c("BTC")) %>% select(open_available_date, mcap),
                       by = c("open_available_date" = "open_available_date"))

df %>% mutate(mcap_pct = mcap.x / mcap_total, mcap_pct_excl = mcap.x / (mcap_total - mcap.y)) %>% filter(currency %in% c("ETH","ADA")) %>% filter(mcap_pct < 0.55) %>% ggplot(mapping = aes(x = open_available_date, y = mcap_pct_excl, fill = currency)) + geom_area(position = "stack")

# # # Kraken Examples ----------------------------------------------------------------
#
#
# key <- "pXWTzAIt2vAUq+L+mkabpYHbYHy/iCd1qZ9+njR2plKEBUAfW+9VATjs"
# secret <- "NlQSADg8lm7p20ef/qGvuL1WggM7g5YnZ7ehEX/AK6zS88wvQENjx+L5LSh2rTglZ3MvUJgTXLArG4mA/V/XGw=="
#
# library(cryptor)
# library(tidyverse)
#
# credentials_kraken(key = key, secret = secret)

#
#
# # pair = "BCHEUR"
# # type = "buy"
# # ordertype = "limit"
# # price = "100"
# # price2 = "90"
# # volume = "0.2"
#
#
# # kraken_add_order(pair = pair, type = type, ordertype = ordertype, price = price, price2 = price2, volume = volume, validate = TRUE)
#
#

# assetpairs <- kraken_assetpairs()
#
# # historical allocation
# ledger <- kraken_ledger()
#
# as_tibble(unique(ledger$asset)) %>%
#   rename(asset = value) %>%
#   left_join(assetpairs %>%
#               filter(quote == "ZEUR") %>%
#               select(base, altname),
#             by = c("asset" = "base")) %>%
#   #filter(altname != "NA") %>%
#   left_join(ledger, by = "asset")
#
# # current allocation
# allocation <- read.csv2(file = "C:/Crypto/0_Accounting/Allocation.csv",stringsAsFactors = FALSE)
#
# balance <- kraken_balance()
# balance <- balance %>% left_join(y = assetpairs %>% filter(quote == "ZEUR") %>% select(base, altname), by = c("asset" = "base"))
# balance <- balance %>% left_join(y = allocation, by = "asset")
#
# kraken_lastprice <- function(pair){
#   as.numeric(kraken_ticker(pair = pair)$result[[1]]$c[[1]])
# }
#
#
# pairs <- balance  %>% filter(altname != "NA") %>% pull(altname)
# prices <- sapply(X = pairs, kraken_lastprice)
# prices <- as_tibble(prices) %>% mutate(asset = names(prices))
#
# balance %>% left_join(y = prices,by = c("altname" = "asset")) %>% mutate(weight = if_else(is.na(weight),0,weight),
#                                                                          value_quote = balance * value,
#                                                                          value = if_else(asset == "ZEUR", 1, value),
#                                                                          assets = sum(value_quote,na.rm = TRUE),
#                                                                          weight_current = value_quote / assets,
#                                                                          value_quote_target = weight * assets,
#                                                                          value_quote_diff = value_quote_target - value_quote,
#                                                                          balance_target = value_quote_target / value,
#                                                                          balance_diff = balance_target - balance) -> balance
#
#
# # prices
