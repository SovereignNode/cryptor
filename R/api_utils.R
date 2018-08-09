

#' API querying function
#'
#' @param config a config file to use in the POST method
#' @param post_data additional post data to use in the POST method
#' @param url a url
#'
#' @return a response
#' @keywords internal
#'
query <- function(url, config, post_data) {

  res <- httr::POST(url = url, config = config, body = post_data)

  if(res$status_code != 200) {
    stop(paste0("API request failed with status code: ", httr::http_status(res)$message))
  }

  httr::content(res)
}
