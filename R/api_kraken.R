
# Kraken UTILS ------------------------------------------------------------



#' Base functionality to call the Kraken API directly
#'
#' @param url Character input URL to query
#' @param sign Boolean indicating if the request is private or public
#' @param args List of additional arguments to query
#'
#' @return Returns the result from the API query to Kraken
#' @export
#'
query_kraken <- function(url, sign = FALSE, args = NULL) {

  if(!is.null(args)){
    args <- args[!unlist(lapply(X = args, FUN = is.null))]
  }

  if(isTRUE(sign)){
    key         <- key_kraken()
    secret      <- secret_kraken()
    nonce       <- as.character(as.numeric(Sys.time()) * 1e+06)
    post_data   <- paste0("nonce=", nonce)
    if (length(args) > 0) {
      post_data <- paste(post_data, paste(paste(names(args), args, sep = "="), collapse = "&"), sep = "&")
    }
    config <- sign_kraken(url = url, secret = secret, key = key, nonce = nonce, post_data = post_data)
  }else{
    post_data <- paste(paste(names(args), args, sep = "="), collapse = "&")
    config <- httr::config()
  }

  query(url = url, config = config, post_data = post_data)

}

#' Signature method for the Kraken API
#'
#' @param url a url
#' @param secret API secret
#' @param key API key
#' @param nonce a nonce value
#' @param post_data additional post data to include in Signature
#' @importFrom digest hmac digest
#' @importFrom base64enc base64decode base64encode
#' @importFrom httr add_headers
#' @return
#'
sign_kraken <- function(url, secret, key, nonce, post_data){

  method_path <- gsub("^.*?kraken.com", "", url)
  sign        <- digest::hmac(key = base64enc::base64decode(secret),
                              object = c(charToRaw(method_path),
                                         digest::digest(object = paste0(nonce, post_data),
                                                algo = "sha256",
                                                serialize = FALSE,
                                                raw = TRUE)),
                              algo = "sha512",
                              raw = TRUE)

  httr::add_headers(c(`API-Key` = key, `API-Sign` = base64enc::base64encode(sign)))
}


# Kraken Public -----------------------------------------------------------


#' get Kraken servertime
#'
#' @return a unix timestamp
#' @export
#'
kraken_servertime <- function(){
  query_kraken(url = "https://api.kraken.com/0/public/Time", sign = FALSE)$result$unixtime
}

#' get available assets
#'
#' @return available assets on Kraken
#' @export
#'
kraken_assets <- function() {
    assets <- query_kraken(url = "https://api.kraken.com/0/public/Assets", sign = FALSE)
    do.call(what = dplyr::bind_rows, args = assets$result)
}

#' Get available assetpairs
#'
#' @param excl_darkpool (optional) exclude darkpool assetpairs from the list (default = TRUE).
#'
#' @return available assetpairs on Kraken
#' @export
#' @importFrom plyr llply
#'
kraken_assetpairs <- function(excl_darkpool = TRUE) {

    assets <- query_kraken(url = "https://api.kraken.com/0/public/AssetPairs",sign = FALSE)
    df     <- do.call(what = dplyr::bind_rows,
                      args = plyr::llply(.data = assets$results,
                                         .fun = function(x) x[unlist(lapply(X = x,
                                                                            FUN = function(x) length(x) == 1))]
                      )
    )
    assetpairs <- names(assets$result)
    df         <- dplyr::bind_cols(assetpair = assetpairs, df)

    if (excl_darkpool){ df <- df %>% dplyr::filter(!grepl(pattern = ".d", x = .$assetpair)) }

    df
}


#' Get candle data
#'
#' @param pairs list of assetpair names or altnames to retrieve OHLC data.
#' @param interval integer corresponding to candle interval
#'
#' @return a dataframe with OHLC data
#' @export
#'
kraken_candles <- function(pairs, interval = 1440) {

  assertthat::assert_that(is_interval_kraken(x = interval))
  assertthat::assert_that(is_pair_kraken(x = pairs))

  pair_data <- dplyr::bind_rows(lapply(X = pairs, FUN = kraken_ohlc, interval = interval))
  attr(x = pair_data, which = "timestamp") <- Sys.time()

  pair_data
}


#' robust retrieval of OHLC for a single pair
#'
#' @param pair assetpair to retrieve OHLC
#' @param interval integer corresponding to time interval
#'
#' @return OHLC dataframe
#'
kraken_ohlc <- function(pair, interval = 1440) {

  # We TRY and check outputs until correct.
  ohlc_data <- NULL
  attempt <- 0
  max_attemps <- 5
  sleep_base <- 2

  while (is.null(ohlc_data) && attempt <= max_attemps) {
    attempt <- attempt + 1
    try(ohlc_data <- kraken_ohlc_simple(pair = pair, interval = interval))
    Sys.sleep(sleep_base * (log(attempt) + 1))
    if (attempt > max_attemps) {stop(paste0("Function execution halted: Too many attempts to query OHLC data on ", pair))}
  }

  ohlc_data
}

#' retrieval of OHLC data for a single pair
#'
#' @param pair an assetpair on Kraken
#' @param interval an interval on Kraken
#'
#' @return OHLC dataframe
#' @importFrom tidyr fill
#'
kraken_ohlc_simple <- function(pair, interval = 1440) {

  url   <- paste0("https://api.kraken.com/0/public/OHLC?pair=", pair, "&interval=", interval)
  frame <- query_kraken(url = url, sign = FALSE)$result[[1]]

  assertthat::not_empty(frame)

  # Convert to proper formatting
  frame <- dplyr::as_tibble(matrix(data = unlist(frame),
                                   nrow = length(frame),
                                   byrow = TRUE)) %>%
    dplyr::mutate(V1 = as.POSIXct(as.numeric(V1), origin = "1970-01-01")) %>%
    dplyr::mutate_if(.predicate = is.character, .funs = dplyr::funs(as.numeric)) %>%
    dplyr::mutate(pair = pair)

  colnames(x = frame) <- c(
    "time",
    "open",
    "high",
    "low",
    "close",
    "vwap",
    "volume",
    "count",
    "asset"
  )

  # Stop if any value in the final row NA
  if (any(is.na(dplyr::slice(.data = frame, n())))) {
    stop(paste0("Execution halted for ", pair, ": final row is NA"))
  }

  # remove duplicate rows and fill down NA's
  frame %>%
    dplyr::distinct() %>%
    tidyr::fill(-time)
}


#' get a ticker
#'
#' @param pair an assetpair
#'
#' @return ticker information for a given assetpair
kraken_ticker <- function(pair) {
  url <- paste0("https://api.kraken.com/0/public/Ticker?pair=", pair)
  ticker <- query_kraken(url = url, sign = FALSE)
  return(ticker)
}

#' Get a pair's last available price
#'
#' @param pair a valid Kraken asset pair.
#'
#' @return last available price for a given pair
#' @export
#'
kraken_lastprice <- function(pair){
  as.numeric(kraken_ticker(pair = pair)$result[[1]]$c[[1]])
}

#' Get order book
#'
#' @param pair asset pair to get market depth for
#' @param count maximum number of asks/bids (optional)
#'
#' @return dataframe of pair name and market depth
#' @export
#'
kraken_orderbook <- function(pair, count = 10) {

  assertthat::is.string(pair)

  url <- paste0("https://api.kraken.com/0/public/Depth?pair=", pair, "&count=", count)
  OrderBook <- query_kraken(url = url, sign = FALSE)$result[[1]]

  # We have to add names to use bind_rows
  for (i in 1:2) {
    names(x = OrderBook[[i]]) <- seq(1, count)
    for (j in 1:count) { names(x = OrderBook[[i]][[j]]) <- c("price", "volume", "time") }
  }

  Asks <- dplyr::bind_rows(OrderBook$asks) %>%
    dplyr::mutate_if(is.character, as.numeric) %>%
    dplyr::mutate(time = as.POSIXct(x = time, origin = "1970-01-01")) %>%
    dplyr::mutate(type = "ask")
  Bids <- dplyr::bind_rows(OrderBook$bids) %>%
    dplyr::mutate_if(is.character, as.numeric) %>%
    dplyr::mutate(time = as.POSIXct(x = time, origin = "1970-01-01")) %>%
    dplyr::mutate(type = "bid")

  orderbook <- dplyr::bind_rows(Asks, Bids) %>%
    dplyr::mutate(pair = pair) %>%
    dplyr::select(pair, type, price, volume, time)

  return(orderbook)
}


#' Get recent spread data
#'
#' @param pair asset pair to get spread data for
#'
#' @return dataframe of pair name and recent spread data
#' @export
#'
kraken_spread <- function(pair) {

  assertthat::is.string(pair)

  url <- paste0("https://api.kraken.com/0/public/Spread?pair=", pair)

  spread <- query_kraken(url = url,sign = FALSE)$result[[1]]
  spread <- magrittr::set_names(x = lapply(X = spread,
                                 FUN = magrittr::set_names,
                                 nm = c("time", "bid", "ask")),
                      nm = paste0(1:length(spread)))

  spread <- do.call(what = dplyr::bind_rows, args = spread) %>%
    dplyr::mutate(time = as.POSIXct(x = time, origin = "1970-01-01")) %>%
    dplyr::mutate_if(.predicate = is.character, .funs = as.numeric) %>%
    dplyr::mutate(pair = pair) %>%
    dplyr::select(pair, time, bid, ask) %>%
    dplyr::mutate(spread = ask - bid) %>%
    dplyr::mutate(mid = (bid + ask) / 2) %>%
    dplyr::arrange(desc(time))

  return(spread)
}


# Kraken Private USER -----------------------------------------------------

#' Get trade balance
#'
#' @return array of asset names and balance amount
#' @export
#'
kraken_balance <- function(){
  balance <- query_kraken(url = "https://api.kraken.com/0/private/Balance", sign = TRUE)
  dplyr::data_frame(names(balance$result),
                    as.vector(unlist(balance$result))) %>%
    magrittr::set_names(c("asset", "balance")) %>%
    dplyr::mutate(balance = as.numeric(balance))
}

#' Get current and target allocation
#'
#' @param target_allocation (required) a target allocation as a dataframe (Nx2) with column names `asset` * `target_weight`
#'
#' @return dataframe of the current allocation on Kraken
#' @export
#'
#' @examples \dontrun{
#' kraken_allocation_current(target_allocation = some.allocation.target)
#' }
kraken_allocation_current <- function(target_allocation){

  balance <- kraken_balance()
  assetpairs <- kraken_assetpairs()

  balance <- balance %>% dplyr::left_join(y = assetpairs %>%
                                            dplyr::filter(quote == "ZEUR") %>%
                                            dplyr::select(base, altname), by = c("asset" = "base")) %>%
    dplyr::left_join(y = target_allocation, by = "asset")


  pairs <- balance  %>% dplyr::filter(altname != "NA") %>% dplyr::pull(altname)
  prices <- sapply(X = pairs, kraken_lastprice)
  prices <- dplyr::as_tibble(prices) %>% dplyr::mutate(asset = names(prices))

  balance %>% dplyr::left_join(y = prices,by = c("altname" = "asset")) %>%
    dplyr::mutate(target_weight = dplyr::if_else(is.na(target_weight), 0, target_weight),
                  value = dplyr::if_else(asset == "ZEUR", 1, value),
                  value_quote = balance * value,
                  assets = sum(value_quote,na.rm = TRUE),
                  weight_current = value_quote / assets,
                  value_quote_target = target_weight * assets,
                  value_quote_diff = value_quote_target - value_quote,
                  balance_target = value_quote_target / value,
                  balance_diff = balance_target - balance)

}


#' Get open orders
#' @param userref (optional) a given 'userref' to query for (default = NULL)
#' @return array of order info in open array with txid as the key
#' @export
#'
kraken_get_open_orders <- function(userref = NULL) {
  open_orders <- query_kraken(url = "https://api.kraken.com/0/private/OpenOrders", sign = TRUE)
  return(open_orders$result$open)
}


#' Get closed orders
#'
#' @return dataframe of order info
#' @export
#' @importFrom utils modifyList
#'
kraken_get_closed_orders <- function(){

  combined <- list()
  offset <- 0
  continue <- TRUE
  while(continue){
    res <-   query_kraken(url = "https://api.kraken.com/0/private/ClosedOrders", sign = TRUE, args = list(ofs = offset))
    combined <- utils::modifyList(x = combined, res)
    offset <- offset + 50
    continue <- (length(res$result$closed) >= 50)
  }
  do.call(what = dplyr::bind_rows,
          args = lapply(X = combined$result$closed,
                        FUN = function(x) unlist(x[sapply(X = x,
                                                          FUN = function(y) !is.list(y))]))) %>%
    dplyr::mutate_at(.vars = dplyr::vars(dplyr::contains("tm")),
                     .funs = dplyr::funs(as.POSIXct(x = as.numeric(.), origin = "1970-01-01")))
}


#' Query orders info
#'
#' @param userref restrict results to given user reference id (optional)
#' @param txid comma delimited list of transaction ids to query info about (20 maximum)
#'
#' @return associative array of orders info
#' @export
kraken_query_orders_info <- function(txid = NULL, userref = NULL) {
  assertthat::is.string(x = txid)
  order_result <- query_kraken(url = "https://api.kraken.com/0/private/QueryOrders",
                               sign = TRUE,
                               args = list(txid = txid, userref = userref))
  return(order_result)
}



#' get ledger information
#'
#' @param simple (optional) Get 'simple' ledger or not (default = TRUE)
#' @return returns the ledger dataframe
#' @importFrom utils modifyList
#' @importFrom tidyr gather
#' @export
#'
kraken_ledger <- function(simple = TRUE) {

  Ledger.comb <- list()
  ofset       <- 0
  continue    <- TRUE
  while (continue) {
    Ledger <- query_kraken(url = "https://api.kraken.com/0/private/Ledgers",
                           sign = TRUE,
                           args = list(ofs = ofset))$result$ledger
    Ledger.comb <- utils::modifyList(Ledger.comb, Ledger)
    ofset       <- ofset + 50
    continue    <- (length(Ledger) >= 50)
  }

  # Bind together
  Ledger <- do.call(what = dplyr::bind_rows, args = Ledger.comb) %>%
    dplyr::mutate(time = as.Date(as.POSIXct(time, origin = "1970-01-01"))) %>%
    dplyr::mutate_at(.vars = c(6:8), .funs = as.numeric) %>%
    magrittr::set_names(c(
      "refid",
      "time",
      "type",
      "aclass",
      "asset",
      "amount",
      "fee",
      "balance"
    ))

  if(isTRUE(simple)){
    return(Ledger)
  }else{
    stop("Simple = FALSE is Not yet implemented")
    Ledger <- Ledger %>%
      dplyr::left_join(y = dplyr::select(kraken_assetpairs(), altname, base), by = c("asset" = "base")) %>%
      dplyr::mutate(altname = dplyr::if_else(asset == "ZEUR", "ZEUR", altname))

    # Let's check if there is no NA's
    if (any(is.na(x = dplyr::pull(Ledger, altname)))) {
      warning("Ledger could not be fully joined by assetpairs. Please inspect manually.")
    }
    # Next add prices
    data_long <- data_wide %>%
      tidyr::gather(key = assetpair, value = close,-time) %>%
      dplyr::mutate(time = as.Date(time, origin = "1970-01-01"))

    Ledger <- Ledger %>%
      dplyr::left_join(y = data_long, by = c("time" = "time", "altname" = "assetpair")) %>%
      dplyr::mutate(amount_net = amount - fee) %>%
      dplyr::mutate(amount_ctv = amount * close) %>%
      dplyr::mutate(fee_ctv = fee * close) %>%
      dplyr::mutate(amount_ctv_net = amount_ctv - fee_ctv) %>%
      dplyr::arrange(time)

    # is_deposits_complete <- function(ledger) {
    #   deposits_indiv <-
    #     as.tibble(read.csv2(file = "C:/Crypto/0_Accounting/Stortingen.csv"))
    #
    #   missing_payments <- ledger %>%
    #     filter(type == "withdrawal" | type == "deposit") %>%
    #     left_join(
    #       y = deposits_indiv %>%
    #         group_by(DATE) %>%
    #         summarise(AMOUNT = sum(AMOUNT)) %>%
    #         mutate(DATE = as.Date(
    #           x = as.character(DATE), origin = "1970-01-01", "%d/%m/%Y"
    #         )),
    #       by = c("time" = "DATE")
    #     ) %>%
    #     select(time, asset, amount_ctv, amount_ctv_net, AMOUNT) %>%
    #     mutate(diff = amount_ctv - AMOUNT) %>% # Compare excluding fee's
    #     pull(diff) %>%
    #     sum()
    #
    #   if (abs(missing_payments) <= 0.1) {
    #     return(TRUE)
    #   } else {
    #     return(FALSE)
    #   }
    # }
    #
    # # We finally do a check on the Ledger to see if the Stortingen file is OK
    # if (!is_deposits_complete(ledger = Ledger)) {
    #   stop("Stortingen.csv is not complete. Please check!")
    # }

    Ledger
  }
}


# Kraken Private Funding --------------------------------------------------

#' Get deposit methods

#'
#' @param aclass (optional) asset class
#' @param asset (required) asset being deposited
#'
#' @return associative array of deposit methods
#' @export
#'
#' @examples
kraken_get_deposit_methods <- function(aclass = NULL, asset){
  query_kraken(url = "https://api.kraken.com/0/private/DepositMethods",
               sign = TRUE,
               args = list(aclass = aclass, asset = asset))$result[[1]]
}

#' Get deposit addresses
#'
#' @param aclass (optional) asset class
#' @param asset (required) asset being deposited
#' @param method (required) name of the deposit method
#' @param new whether or not to generate a new address (optional. default = false)
#'
#' @return associative array of deposit addresses
#' @export
#'
#' @examples
kraken_get_deposit_address <- function(aclass = NULL, asset, method, new = NULL){
  query_kraken(url = "https://api.kraken.com/0/private/DepositAddresses",
               sign = TRUE,
               args = list(aclass = aclass, asset = asset, method = method, new = new))$result[[1]]
}

#' Get withdrawal information
#'
#' @param aclass (optional) asset class
#' @param asset asset being withdrawn
#' @param key withdrawal key name, as set up on your account
#' @param amount amount to withdraw
#'
#' @return associative array of withdrawal info:
#' @export
#'
#' @examples
kraken_get_withdrawal_info <- function(aclass = NULL, asset, key, amount){
  query_kraken(url = "https://api.kraken.com/0/private/WithdrawInfo",
               sign = TRUE,
               args = list(aclass = aclass, asset = asset, key = key, amount = amount))
}

#' Withdraw funds
#'
#' @param aclass (optional) asset class
#' @param asset asset being withdrawn
#' @param key withdrawal key name, as set up on your account
#' @param amount amount to withdraw, including fee's
#'
#' @return associative array of withdrawal transaction:
#' @export
#'
#' @examples
kraken_withdraw_funds <- function(aclass = NULL, asset, key, amount){
  query_kraken(url = "https://api.kraken.com/0/private/Withdraw",
               sign = TRUE,
               args = list(aclass = aclass, asset = asset, key = key, amount = amount))
}
# Kraken Private TRADING --------------------------------------------------


#' Add standard order
#'
#' @param args list of arguments to include in API call
#'
#' @return order description info and array of transaction ids for order (if order was added successfully)
#' @keywords internal
#'
kraken_add_order_internal <- function(args){
  # Use with caution - make sure args is correct !
  query_kraken(url = "https://api.kraken.com/0/private/AddOrder",
               sign = TRUE,
               args = args)
}



#' Add standard order
#'
#' @param type type of order (buy/sell)
#' @param price price (optional.  dependent upon ordertype)
#' @param price2 secondary price (optional.  dependent upon ordertype)
#' @param volume order volume in lots
#' @param pair asset pair
#' @param ordertype order type:
#' @param validate validate inputs only.  do not submit order (optional)
#'
#' @return void
#' @export
#'
kraken_add_order <- function(pair, type, ordertype, price, price2, volume, validate = TRUE){

  args <- list()

  # Required:
  args$pair <- pair
  args$type <- type
  args$ordertype <- ordertype
  args$volume <- volume

  if(!all(as.vector(sapply(X = args, FUN = is.character)))){stop("Inputs need to be strings")}

  # Optionals:
  if((ordertype != "market") & (ordertype != "settle-position")){
    assertthat::not_empty(price)
    args$price <- price
  }
  if(ordertype %in% c("stop-loss-profit",
                      "stop-loss-profit-limit",
                      "stop-loss-limit",
                      "take-profit-limit",
                      "trailing-stop-limit",
                      "stop-loss-and-limit")){
    assertthat::not_empty(price2)
    args$price2 <- price2
  }
  if(is.logical(validate)){
    if(validate){
      args$validate <- "true"
    }else{
      args$validate <- "false"
    }
  }


  if(isTRUE(validate)){
    check <- kraken_add_order_internal(args = args)
    if(assertthat::not_empty(check$error)){stop(paste0("Invalid call: ",check$error))}
    cat(check$result$descr$order)
    confirm <- readline(prompt = "Press Y to confirm: ")
    if(toupper(confirm) == "Y"){
      args <- args[!(names(args)=="validate")]
      kraken_add_order_internal(args = args)
    }
  }else{
    kraken_add_order_internal(args = args)
  }
}


#' Cancel open order
#'
#' @param txid transaction id
#'
#' @return list of number of orders canceled and pending information
#' @export
#'
kraken_cancel_open_order <- function(txid) {
  assertthat::not_empty(txid)
  query_kraken(url = "https://api.kraken.com/0/private/CancelOrder", sign = TRUE, args = list(txid = txid))
}


#' Cancel open orders or a selection of order ids
#'
#' @param ids vector or list of txid's to close (optional)
#'
#' @return list of return values from kraken_cancel_open_order()
#' @export
#'
kraken_cancel_open_orders <- function(ids) {
  assertthat::not_empty(ids)
  lapply(ids, kraken_cancel_open_order)
}



# LOGIC -------------------------------------------------------------------

#' Rebalance the portfolio on Kraken
#'
#' @param allocation_summary the current allocation (returned from kraken_allocation_current())
#'
#' @return void
#' @export
#'
kraken_rebalance_portfolio <- function(allocation_summary) {

  answer <- readline(prompt = "Rebalancing portfolio. Press Y to continue: ")
  if (toupper(answer) != "Y") { stop("User aborted") }

  # if current allocation is not provided then query it...

  to_allocate <- allocation_summary %>%
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
    kraken_close_open_orders()

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
      kraken_add_order_until_complete(pair = Pair,
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
#' @return void
#'
kraken_add_order_until_complete <- function(pair, type, volume) {
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
      PlacedOrder <- kraken_add_order(account = "fcd",
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
        kraken_close_open_orders(refid = OrderID)
        PlacedOrder <- kraken_add_order(account = "fcd",
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


