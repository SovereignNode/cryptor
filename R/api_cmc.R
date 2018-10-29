#' Title
#'
#' @param top_n
#'
#' @return
#' @export
#'
#' @examples
cmc_top_n <- function(top_n = 40){
    url <- paste0("https://api.coinmarketcap.com/v2/ticker/?convert=USD&limit=",as.character(top_n))
    coin_market_cap <- jsonlite::fromJSON(txt = url)

    combined_frame <- dplyr::data_frame()
    for (i in 1:top_n){
        name <- coin_market_cap$data[[i]]$name
        symbol <- coin_market_cap$data[[i]]$symbol
        rank <- coin_market_cap$data[[i]]$rank
        price <- coin_market_cap$data[[i]]$quotes$USD$price
        mcap <- coin_market_cap$data[[i]]$quotes$USD$market_cap
        tbl <- dplyr::data_frame(name = name,
                          symbol = symbol,
                          rank = rank,
                          price_usd = price,
                          mcap_usd = mcap)
        combined_frame <- dplyr::bind_rows(combined_frame, tbl)
    }
    return(combined_frame)
}
