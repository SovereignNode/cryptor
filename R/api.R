# KRAKEN ----------------------------------------------------------------

#' Functionality to interface with Kraken API
#'
#' @param url Character input URL to query
#' @param args List of additional arguments to query
#' @param private Boolean value indicating a private or public query
#' @param account Pre-defined character input when using a private query
#'
#' @return Returns the result from the API query to Kraken
#' @export
#'
query_kraken <-
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

#' Query Assets available on Kraken
#'
#' @return available assets on Kraken
#' @export
#'
query_kraken_assets <- function() {
    assets <- query_kraken(url = "https://api.kraken.com/0/public/Assets")
    df     <- do.call(dplyr::bind_rows,
                      assets$result)
  return(df)
}

#' Query Assetpairs available on Kraken
#'
#' @return available assetpairs on Kraken
#' @export
#' @importFrom magrittr "%>%"
#'
query_kraken_assetpairs <-
  function(incl_darkpool = FALSE) {
    assets <- query_kraken(url = "https://api.kraken.com/0/public/AssetPairs")
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

    if (incl_darkpool){ df <- df %>% filter(!grepl(pattern = ".d", x = .$assetpair)) }

    return(df)
}


#' Query OHLC data from Kraken
#'
#' @param pairs list of assetpairs to retrieve OHLC data
#' @param interval integer corresponding to time interval
#'
#' @return a dataframe with OHLC data
#' @export
#'
query_kraken_prices <- function(pairs, interval = 1440) {
  if (!is.numeric(interval)) {
    stop("interval should be a numeric")
  }

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


#' Robust extraction of OHLC prices from Kraken
#'
#' @param pair assetpair to retrieve OHLC
#' @param interval integer corresponding to time interval
#'
#' @return
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
    frame <- query_kraken(url = url)$result[[1]]

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

    try(ohlc_data <- query())

    if (attempt > max_attemps) {
      stop(paste0(
        "Function execution halted: Too many attempts to query OHLC data on ",
        pair
      ))
    }
  }

  return(ohlc_data)
}


#' Query Kraken Ledger
#'
#' @param data_wide Historical dataframe in 'wide' format
#'
#' @return returns the ledger dataframe
#' @export
query_kraken_ledger <- function(data_wide) {

  # Initial query to Ledger
  Ledger <-
    query_kraken(url = "https://api.kraken.com/0/private/Ledgers",
                 private = TRUE,
                 account = "fcd")$result$ledger

  # Continue using 'offset' untill Ledger < 50 items.
  Ledger.comb <- Ledger
  ofset <- 0
  while (length(Ledger) >= 50) {
    ofset = ofset + 50
    Ledger = query_kraken(
      url = "https://api.kraken.com/0/private/Ledgers",
      private = TRUE,
      account = "fcd",
      args = list(ofs = ofset)
    )$result$ledger
    Ledger.comb = modifyList(Ledger.comb, Ledger)
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
    left_join(y = select(asset_pairs, altname, base),
              by = c("asset" = "base")) %>%
    mutate(altname = if_else(asset == "ZEUR", "ZEUR", altname))

  # Let's check if there is no NA's
  if (any(is.na(x = pull(Ledger, altname)))) {
    stop("Ledger could not be fully joined by assetpairs. Please inspect.")
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


#' Query open orders
#'
#' @return all open orders
#' @export
query_kraken_open_orders <- function() {
  open_orders <-
    query_kraken(url = "https://api.kraken.com/0/private/OpenOrders",
                 private = TRUE,
                 account = "fcd")
  # Maybe some checks on error status
  return(open_orders$result$open)
}

#' Query a single order
#'
#' @param order_id an orderid
#'
#' @return result for order
#' @export
query_kraken_order <- function(order_id) {
  order_result <-
    query_kraken(
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
  OrderBook <- query_kraken(url = url)$result[[1]]

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
  ticker <- query_kraken(url = url)
  return(ticker)
}

#' Query spread information from Kraken
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

  spread <- query_kraken(url = url)$result[[1]]
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

      #KrakenPair <- KrakenDictionary(pair = Pair)
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
      PlacedOrder <-
        query_kraken(
          url = "https://api.kraken.com/0/private/AddOrder",
          account = "fcd",
          private = TRUE,
          args = list(
            pair = pair,
            type = type,
            ordertype = "limit",
            price = BidPrice,
            # Highest bid price
            volume = volume
          )
        )
      BidPriceOrig <- BidPrice # store this
      OrderID <- PlacedOrder$result$txid
      first <- FALSE
      cat(PlacedOrder$result$descr, "\n")
    } else{
      if (BidPrice == BidPriceOrig) {
        # nothing to do
      } else{
        close_open_orders(refid = OrderID)
        PlacedOrder <-
          query_kraken(
            url = "https://api.kraken.com/0/private/AddOrder",
            account = "fcd",
            private = TRUE,
            args = list(
              pair = pair,
              type = type,
              ordertype = "limit",
              price = BidPrice,
              volume = volume
            )
          )
        BidPriceOrig <- BidPrice
        OrderID <- PlacedOrder$result$txid
        cat("Updated order: ", PlacedOrder$result$descr, "\n")
      }
    }
    #AlreadyOpen <- GetOpenOrders()

    Sys.sleep(1)

    # Check for complete
    # WHAT IF ONLY PARTIALLY COMPLETE?
    QueryOrder <- query_kraken_order(order_id = OrderID)


    QueryOrder_Status <- QueryOrder$result[[1]]$status
    QueryOrder_Volume <- QueryOrder$result[[1]]$vol
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

#' Close all open orders
#'
#' @param refids vector of refids to close (optional)
#'
#' @return nothing
#' @export
close_open_orders <- function(refids = NULL) {
  close_open_order <- function(refids) {
    if (is_null(refids)) {
      stop("No refid given to cancel")
    }
    for (i in refids) {
      query_kraken(
        url = "https://api.kraken.com/0/private/CancelOrder",
        account = "fcd",
        private = TRUE,
        args = list(txid = i)
      )
      cat("Order", i, " closed.\n")
    }
  }

  if (!is_null(refids)) {
    close_open_order(refids = refids)

  } else{
    # if NULL then get all refid's to cancel
    open_orders <- query_kraken_open_orders()

    if (is_empty(open_orders)) {
      cat("Nothing to close.\n")
    } else{
      refids <- names(open_orders)
      close_open_order(refids = refids)
    }
  }
}
