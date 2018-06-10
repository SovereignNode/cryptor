# KRAKEN ----------------------------------------------------------------

#' Base functionality to call the Kraken API directly
#'
#' @param url Character input URL to query
#' @param args List of additional arguments to query
#' @param private Boolean value indicating a private or public query
#' @param account Pre-defined character input when using a private query
#'
#' @return Returns the result from the API query to Kraken
#' @export
#'
kraken_query <-
  function(url,
           args = NULL,
           private = FALSE,
           account = NULL) {
    if (!private) {
      if (!is.null(account)){
        stop("Account should only be specified when using private method...")
      }
      Rbitcoin::market.api.query.kraken(url = url,
                              req = args)
    } else{
      if (is_null(account)) {
        stop("Use of API for private method requires a valid account...")
      }
      if (account == "fcd") {
        key <- "pXWTzAIt2vAUq+L+mkabpYHbYHy/iCd1qZ9+njR2plKEBUAfW+9VATjs"
        secret <-
          "NlQSADg8lm7p20ef/qGvuL1WggM7g5YnZ7ehEX/AK6zS88wvQENjx+L5LSh2rTglZ3MvUJgTXLArG4mA/V/XGw=="
      }
      else{
        stop("Input 'account' not recognized")
      }
      Rbitcoin::market.api.query.kraken(
        url = url,
        key = key,
        secret = secret,
        req = args
      )
    }
  }

#' get available assets
#'
#' @return available assets on Kraken
#' @export
#'
kraken_assets <- function() {
    assets <- kraken_query(url = "https://api.kraken.com/0/public/Assets")
    df     <- do.call(dplyr::bind_rows,
                      assets$result)
  return(df)
}

#' get available assetpairs
#'
#' @return available assetpairs on Kraken
#' @export
#'
kraken_assetpairs <-
  function(excl_darkpool = TRUE) {

    assets <- kraken_query(url = "https://api.kraken.com/0/public/AssetPairs")
    df     <- do.call(dplyr::bind_rows,
                      plyr::llply(
                        .data = assets$result,
                        .fun = function(x)
                          x[unlist(lapply(
                            X = x,
                            FUN = function(x)
                              length(x) == 1
                          ))]
                      ))
    assetpairs <- names(assets$result)
    df         <- dplyr::bind_cols(assetpair = assetpairs, df)

    if (excl_darkpool){ df <- df %>% dplyr::filter(!grepl(pattern = ".d", x = .$assetpair)) }
    return(df)
}


#' get candle data
#'
#' @param pairs list of assetpair names or altnames to retrieve OHLC data.
#' @param interval integer corresponding to candle interval
#'
#' @return a dataframe with OHLC data
#' @export
#'
kraken_candles <- function(pairs, interval = 1440) {

  # if (!(interval %in% c(1, 5, 15, 30, 60, 240, 1440, 10080, 21600))) {
  #   stop("interval should be a numeric value corresponding the number of minutes of a candle (1, 5, 15, 30, 60, 240, 1440).")
  # }

  assertthat::assert_that(is_interval_kraken(x = interval))
  assertthat::assert_that(is_pair_kraken(x = pairs))

  # Get the data from the Kraken API
  pair_data <- dplyr::bind_rows(lapply(
    X = pairs,
    FUN = query_kraken_ohlc,
    interval = interval
  ))

  # Add the timestamp as metadata
  attr(x = pair_data, which = "timestamp") <- Sys.time()

  return(pair_data)
}


#' (internal) get ohlc data
#'
#' @param pair assetpair to retrieve OHLC
#' @param interval integer corresponding to time interval
#'
#' @return
#' @import tidyverse
#'
query_kraken_ohlc <- function(pair, interval) {

  # Function to get OHLC data
  query <- function() {
    # Construct URL
    url <-
      paste0("https://api.kraken.com/0/public/OHLC?pair=",
             pair,
             "&interval=",
             interval)

    # Query Kraken API
    frame <- kraken_query(url = url)$result[[1]]

    # Break if frame is NULL
    if (is.null(frame)) {
      stop(paste0("Execution halted for ", pair, ": no frame found was found"))
    }

    # Convert to proper formatting
    frame <-
      as.tibble(matrix(
        data = unlist(frame),
        nrow = length(frame),
        byrow = TRUE
      )) %>%
      'colnames<-'(c(
        "time",
        "open",
        "high",
        "low",
        "close",
        "vwap",
        "volume",
        "count"
      )) %>%
      mutate(time = as.POSIXct(as.numeric(time), origin = "1970-01-01")) %>%
      mutate_if(.predicate = is.character, .funs = funs(as.numeric)) %>%
      mutate(pair = pair)

    # Stop if any value in the final row NA
    if (any(is.na(slice(.data = frame, n())))) {
      stop(paste0("Execution halted for ", pair, ": final row is NA"))
    }

    # remove duplicate rows and fill down NA's
    frame <- frame %>%
      distinct() %>%
      fill(-time)

    # Return the frame
    return(frame)
  }


  # We TRY and check outputs until correct.
  ohlc_data <- NULL
  attempt <- 1
  max_attemps <- 5
  sleep_base <- 2

  while (is.null(ohlc_data) && attempt <= max_attemps) {
    Sys.sleep(sleep_base * (log(attempt) + 1))
    attempt <- attempt + 1

    try(ohlc_data <- query(), silent = TRUE)

    if (attempt > max_attemps) {
      stop(paste0(
        "Function execution halted: Too many attempts to query OHLC data on ",
        pair
      ))
    }
  }

  return(ohlc_data)
}


#' get ledger information
#'
#' @return returns the ledger dataframe
#' @export
#' @import tidyverse
#'
query_kraken_ledger <- function() {

  # Query ledger 50 items at a time
  Ledger.comb <- list()
  ofset <- 0
  while (length(Ledger) >= 50) {

    Ledger <- kraken_query(url = "https://api.kraken.com/0/private/Ledgers",
                           private = TRUE,
                           account = "fcd",
                           args = list(ofs = ofset))$result$ledger
    Ledger.comb = modifyList(Ledger.comb, Ledger)
    ofset <- ofset + 50
  }

  # Bind together
  Ledger <- do.call(what = bind_rows, args = Ledger.comb) %>%
    mutate(time = as.Date(as.POSIXct(time, origin = "1970-01-01"))) %>%
    mutate_at(.vars = c(6:8), .funs = as.numeric) %>%
    set_names(c(
      "refid",
      "time",
      "type",
      "aclass",
      "asset",
      "amount",
      "fee",
      "balance"
    )) %>%
    left_join(y = select(query_kraken_assetpairs(), altname, base),
              by = c("asset" = "base")) %>%
    mutate(altname = if_else(asset == "ZEUR", "ZEUR", altname))

  # Let's check if there is no NA's
  if (any(is.na(x = pull(Ledger, altname)))) {
    warning("Ledger could not be fully joined by assetpairs. Please inspect manually.")
  }


  # Next add prices
  data_long <- data_wide %>%
    gather(key = assetpair, value = close,-time) %>%
    mutate(time = as.Date(time, origin = "1970-01-01"))

  Ledger <-
    Ledger %>% left_join(y = data_long,
                         by = c("time" = "time", "altname" = "assetpair")) %>%
    mutate(amount_net = amount - fee) %>%
    mutate(amount_ctv = amount * close) %>%
    mutate(fee_ctv = fee * close) %>%
    mutate(amount_ctv_net = amount_ctv - fee_ctv) %>%
    arrange(time)

  is_deposits_complete <- function(ledger) {
    deposits_indiv <-
      as.tibble(read.csv2(file = "C:/Crypto/0_Accounting/Stortingen.csv"))

    missing_payments <- ledger %>%
      filter(type == "withdrawal" | type == "deposit") %>%
      left_join(
        y = deposits_indiv %>%
          group_by(DATE) %>%
          summarise(AMOUNT = sum(AMOUNT)) %>%
          mutate(DATE = as.Date(
            x = as.character(DATE), origin = "1970-01-01", "%d/%m/%Y"
          )),
        by = c("time" = "DATE")
      ) %>%
      select(time, asset, amount_ctv, amount_ctv_net, AMOUNT) %>%
      mutate(diff = amount_ctv - AMOUNT) %>% # Compare excluding fee's
      pull(diff) %>%
      sum()

    if (abs(missing_payments) <= 0.1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  # We finally do a check on the Ledger to see if the Stortingen file is OK
  if (!is_deposits_complete(ledger = Ledger)) {
    stop("Stortingen.csv is not complete. Please check!")
  }

  return(Ledger)
}


#' get all open orders
#'
#' @return all open orders
#' @export
query_kraken_open_orders <- function() {
  open_orders <-
    kraken_query(url = "https://api.kraken.com/0/private/OpenOrders",
                 private = TRUE,
                 account = "fcd")
  # Maybe some checks on error status
  return(open_orders$result$open)
}

#' get a single open order by order_id
#'
#' @param order_id an orderid
#'
#' @return result for order
#' @export
query_kraken_order <- function(order_id) {
  order_result <-
    kraken_query(
      url = "https://api.kraken.com/0/private/QueryOrders",
      account = "fcd",
      private = TRUE,
      args = list(txid = order_id)
    )
  return(order_result)
}

#' Query orderbook for a given pair
#'
#' @param pair an assetpair
#' @param count number of lines to retrieve
#'
#' @return bid and ask orderbook
#' @export
query_kraken_orderbook <- function(pair, count = 10) {

  if (is_null(pair)) {
    stop("Please provide a pair...")
  }

  url <-
    paste0("https://api.kraken.com/0/public/Depth?pair=",
           pair,
           "&count=",
           count)
  OrderBook <- kraken_query(url = url)$result[[1]]

  # We have to add names to use bind_rows
  for (i in 1:2) {
    names(x = OrderBook[[i]]) <- seq(1, count)
    for (j in 1:count) {
      names(x = OrderBook[[i]][[j]]) <- c("price", "volume", "time")
    }
  }

  Asks <- bind_rows(OrderBook$asks) %>%
    mutate_if(is.character, as.numeric) %>%
    mutate(time = as.POSIXct(x = time, origin = "1970-01-01")) %>%
    mutate(type = "ask")
  Bids <- bind_rows(OrderBook$bids) %>%
    mutate_if(is.character, as.numeric) %>%
    mutate(time = as.POSIXct(x = time, origin = "1970-01-01")) %>%
    mutate(type = "bid")

  orderbook <- bind_rows(Asks, Bids) %>%
    mutate(pair = pair) %>%
    select(pair, type, price, volume, time)

  return(orderbook)
}

#' Query a ticker
#'
#' @param pair an assetpair
#'
#' @return ticker information for a given assetpair
query_kraken_ticker <- function(pair) {
  url <- paste0("https://api.kraken.com/0/public/Ticker?pair=", pair)
  ticker <- kraken_query(url = url)
  return(ticker)
}

#' get spread information
#'
#' @param pair an assetpair
#'
#' @return dataframe with spread information
#' @export
query_kraken_spread <- function(pair) {

  if (is_null(pair)) {
    stop("Please provide a pair...")
  }

  url <- paste0("https://api.kraken.com/0/public/Spread?pair=", pair)

  spread <- kraken_query(url = url)$result[[1]]
  spread <- set_names(x = lapply(
    X = spread,
    FUN = set_names,
    nm = c("time", "bid", "ask")
  ),
  nm = paste0(1:length(spread)))

  spread <- do.call(what = bind_rows, args = spread) %>%
    mutate(time = as.POSIXct(x = time, origin = "1970-01-01")) %>%
    mutate_if(.predicate = is.character, .funs = as.numeric) %>%
    mutate(pair = pair) %>%
    select(pair, time, bid, ask) %>%
    mutate(spread = ask - bid) %>%
    mutate(mid = (bid + ask) / 2) %>%
    arrange(desc(time))

  return(spread)
}


#' Rebalance the portfolio on Kraken
#'
#' @param current_allocation the current allocation
#'
#' @return nothing
#' @export
rebalance_kraken_portfolio <- function(current_allocation) {

  answer <- readline(prompt = "Rebalancing portfolio. Press Y to continue: ")
  if (toupper(answer) != "Y") { stop("User aborted") }

  # if current allocation is not provided then query it...

  to_allocate <- current_allocation %>%
    mutate(TYPE = if_else(CCY_DIFF_FINAL < 0 , "sell", "buy")) %>%
    filter(CCY_DIFF_FINAL != "NA") %>%
    select(ASSET, TYPE, CCY_DIFF_FINAL) %>%
    arrange(desc(TYPE)) %>% # Sells first
    filter(ASSET != "ZEUR")

  # Continue with allocation
  if (nrow(to_allocate) == 0) {
    cat("Nothing to do.\n")
  } else{
    # Before we begin we close all open orders
    close_open_orders()

    # Loop over To Allocate (Sells first)
    for (j in 1:nrow(to_allocate)) {
      Pair <- as.character(to_allocate[j, 1])

      Type <- to_allocate %>%
        filter(ASSET == Pair) %>%
        select(TYPE) %>%
        as.character()

      Volume <- to_allocate %>%
        filter(ASSET == Pair) %>%
        select(CCY_DIFF_FINAL) %>%
        as.numeric() %>%
        abs() %>%
        as.character()

      # Do some checks here

      # Place order and follow until complete
      add_order_until_complete(pair = Pair,
                               type = Type,
                               volume = Volume)

    }
  }
}


#' Add orders until rebalanced
#'
#' @param pair an assetpair
#' @param type a type of order (buy or sell)
#' @param volume volume to execute
#'
#' @return nothing
add_order_until_complete <- function(pair, type, volume) {
  # Place on the highest BID until executed
  complete <- FALSE
  first <- TRUE

  while (!complete) {
    # Get most recent BID price (Sells should happen immediatly)
    OrderBook <- query_kraken_orderbook(pair = pair)
    BidPrice <- OrderBook %>%
      filter(type == "bid") %>%
      filter(volume >= volume) %>%
      slice(1) %>%
      pull(var = price)

    # Since this is a loop, danger to create multiple orders here !
    # First time in this loop there will be NO open orders.
    if (first) {
      PlacedOrder <- add_kraken_order(account = "fcd",
                                      assetpair = pair,
                                      direction = type,
                                      type = "limit",
                                      price = BidPrice,
                                      volume = volume)
      BidPriceOrig <- BidPrice # store this
      OrderID <- PlacedOrder$result$txid
      first <- FALSE
      cat(PlacedOrder$result$descr, "\n")
    } else{
      if (BidPrice == BidPriceOrig) {
        # nothing to do
      } else{
        close_open_orders(refid = OrderID)
        PlacedOrder <- add_kraken_order(account = "fcd",
                                        assetpair = pair,
                                        direction = type,
                                        type = "limit",
                                        price = BidPrice,
                                        volume = volume)
        BidPriceOrig <- BidPrice
        OrderID <- PlacedOrder$result$txid
        cat("Updated order: ", PlacedOrder$result$descr, "\n")
      }
    }

    Sys.sleep(1)

    # Check for complete
    # WHAT IF ONLY PARTIALLY COMPLETE?
    QueryOrder           <- query_kraken_order(order_id = OrderID)
    QueryOrder_Status    <- QueryOrder$result[[1]]$status
    QueryOrder_Volume    <- QueryOrder$result[[1]]$vol
    QueryOrder_Volume_ex <- QueryOrder$result[[1]]$vol_ex

    if (QueryOrder_Status == "closed") {
      if (QueryOrder_Volume == QueryOrder_Volume_ex) {
        complete <- TRUE
        cat("Order fully executed.\n")
      } else{
        cat("Order partially executed.\n")
        volume <-
          as.character(as.numeric(QueryOrder_Volume_ex) - as.numeric(QueryOrder_Volume))
      }
    }
  }
}

#' add order
#'
#' @param account a valid account (see kraken_query())
#' @param assetpair a valid assetpair
#' @param direction trade direction (buy or sell)
#' @param type an ordertype
#' @param price a price for the order
#' @param volume volume to trade
#'
#' @return result of AddOrder query on Kraken
#' @export
#'
add_kraken_order <- function(account, assetpair, direction, type, price, volume){

  o <- kraken_query(
    url = "https://api.kraken.com/0/private/AddOrder",
    account = account,
    private = TRUE,
    args = list(
      pair = assetpair,
      type = direction,
      ordertype = type,
      price = price,
      volume = volume
    )
  )
  return(o)
}

#' Close all open orders or a selection of order ids
#'
#' @param refids vector of refids to close (optional)
#'
#' @return nothing
#' @export
close_kraken_open_orders <- function(ids = NULL) {

  close_open_order <- function(refids) {
    if (is_null(refids)) {
      stop("No refid given to cancel")
    }
    for (i in refids) {
      kraken_query(
        url = "https://api.kraken.com/0/private/CancelOrder",
        account = "fcd",
        private = TRUE,
        args = list(txid = i)
      )
      cat("Order", i, " closed.\n")
    }
  }

  if (!is_null(ids)) {
    close_open_order(refids = ids)

  } else{
    # if NULL then get all refid's to cancel
    open_orders <- query_kraken_open_orders()

    if (is_empty(open_orders)) {
      cat("Nothing to close.\n")
    } else{
      ids <- names(open_orders)
      close_open_order(refids = ids)
    }
  }
}



# BITSCREENER -------------------------------------------------------------


