# credentials_nomics(key = "9bd54db51ed9bf555dad7a2a457294ff")
# date_seq <- seq.Date(from = lubridate::ymd(20180101),to = lubridate::ymd(20180810),by = 1)
#
#
# available_supply <- lapply(X = date_seq, FUN = nomics_supplies_interval, end = lubridate::ymd(20180810))
# supply <- bind_rows(available_supply) %>%
#   select(currency, open_available, open_timestamp) %>%
#   rename(open_available_timestamp = open_timestamp) %>%
#   mutate(open_available_date = as.Date(as.POSIXct(open_available_timestamp, format="%Y-%m-%dT%H:%M:%SZ"))) %>%
#   select(currency, open_available, open_available_date)
#
# available_opens <- lapply(X = date_seq, FUN = nomics_currencies_interval, end = lubridate::ymd(20180810))
# opens <- bind_rows(available_opens) %>%
#   select(currency, open, open_timestamp) %>%
#   mutate(open_date = as.Date(as.POSIXct(open_timestamp, format="%Y-%m-%dT%H:%M:%SZ"))) %>%
#   select(currency, open, open_date)
#
#
# combined <- left_join(x = supply, y = opens, by = c("currency" = "currency", "open_available_date" = "open_date"))
# combined <- combined %>% mutate(open_available = as.numeric(open_available),
#                                 open = as.numeric(open)) %>%
#   select(open_available_date, everything()) %>%
#   arrange(open_available_date) %>%
#   mutate(mcap = open_available * open)
#
# currencies <- combined %>%
#   filter(open_available_date == lubridate::ymd(20180805)) %>%
#   filter(mcap != "NA") %>%
#   arrange(desc(mcap)) %>%
#   top_n(50) %>%
#   pull(currency)
#
# combined <- combined %>% filter(currency %in% currencies)
#
# write.csv2(x = combined, file = "C:/Crypto/5_Data/Market_cap_summary.csv")
#
# summed_caps <- bind_rows(nomics_marketcap(start = lubridate::ymd(20180101), end = lubridate::ymd(20180810))) %>%
#   mutate(timestamp = as.Date(as.POSIXct(timestamp, format="%Y-%m-%dT%H:%M:%SZ")),
#          mcap_total = as.numeric(market_cap))
#
# df <- combined %>%
#   left_join(y = summed_caps, by = c("open_available_date" = "timestamp"))
#
#
# df <- df %>% left_join(y = df %>% filter(currency %in% c("BTC")) %>% select(open_available_date, mcap),
#                        by = c("open_available_date" = "open_available_date"))
#
# df %>% mutate(mcap_pct = mcap.x / mcap_total, mcap_pct_excl = mcap.x / (mcap_total - mcap.y)) %>% filter(currency %in% c("ETH","ADA")) %>% filter(mcap_pct < 0.55) %>% ggplot(mapping = aes(x = open_available_date, y = mcap_pct_excl, fill = currency)) + geom_area(position = "stack")
#
