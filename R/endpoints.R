

#' @export
ping <- function() bi_get(path = "ping")

#' @export
coin_server_time <- function() from_unix(stringr::str_remove(bi_get(path = "time")$serverTime, "\\d{3}$"))

#' @export
coin_system_status <- function() tibble::as_tibble(bi_get(type = "withdraw", path = "systemStatus.html"))

#' @export
coin_exchange_info <- function(symbol = NULL){

  bi_get(path = "exchangeInfo") %>%
    parse_exchange_info(symbol = symbol) %>%
    tidyr::unnest(symbols)

}

#' @export
coin_prices <- function(symbol = NULL){

  out <- bi_get(path = "ticker/allPrices") %>%
    purrr::map_dfr(tibble::as_tibble) %>%
    dplyr::arrange(symbol) %>%
    dplyr::mutate(price = as.numeric(price))

  if(!is.null(symbol)){
    out <- out %>%
      dplyr::filter(stringr::str_detect(symbol, !!symbol))
  }

  return(out)
}

#' @export
coin_book <- function(symbol = NULL, limit = 100){

  if(is.null(symbol)){

    out <- bi_get(path = "ticker/allBookTickers") %>%
      purrr::map_dfr(tibble::as_tibble)

  } else {

    out <-  symbol %>%
      check_symbol(limit = limit) %>%
      purrr::map_dfr(~{
        bi_get(path = "depth", data = .x) %>%
          parse_book %>%
          dplyr::mutate(symbol = .x$symbol)
      })
  }

  out %>%
    janitor::clean_names() %>%
    dplyr::mutate(timestamp = lubridate::now(tz = "EST"))
}

#' @export
coin_all_orders <- function(symbol, limit = 1000){
  symbol %>%
    purrr::map_dfr(~{
      req <- bi_get(path = "allOrders", type = "api", signed = T,
                    data = tibble::tibble(symbol = .x, limit = limit)) %>%
        purrr::map_dfr(tibble::as_tibble)

      if(nrow(req) == 0) return(tibble::tibble())

      req %>%
        janitor::clean_names() %>%
        dplyr::transmute(symbol, order_id, client_order_id,
                         time = lubridate::as_datetime(time/1000),
                         volume = as.numeric(orig_qty), quote_volume = as.numeric(cummulative_quote_qty),
                         avg_price = quote_volume/volume,
                         type, side)
    })
}

#' @export
coin_open_orders <- function(symbol = NULL){

  if(is.null(symbol)){

    req <- bi_get(path = "openOrders", type = "api", signed = T) %>%
      purrr::map_dfr(tibble::as_tibble)

    if(nrow(req) == 0) return(tibble::tibble())

    req %>%
      janitor::clean_names()

  } else {

    symbol %>%
      map_dfr(~{
        req <- bi_get(path = "openOrders", type = "api", signed = T,
                      data = tibble::tibble(symbol = .x)) %>%
          purrr::map_dfr(tibble::as_tibble)

        if(nrow(req) == 0) return(tibble::tibble())

        req %>%
          janitor::clean_names()
      })
  }
}

#' @export
coin_my_trades <- function(symbol, limit = 1000){
  symbol %>%
    map_dfr(~{
      req <- bi_get(path = "myTrades", type = "api", signed = T,
                    data = tibble::tibble(symbol = .x, limit = limit)) %>%
        purrr::map_dfr(tibble::as_tibble)

      if(nrow(req) == 0) return(tibble::tibble())

      req %>%
        janitor::clean_names() %>%
        dplyr::transmute(symbol, binance_order_id = order_id,
                         time = lubridate::as_datetime(time/1000),
                         volume = as.numeric(qty), quote_volume = as.numeric(quote_qty),
                         price = as.numeric(price),
                         com = as.numeric(commission), com_asset = commission_asset,
                         side = ifelse(is_buyer, "BUY", "SELL"),
                         maker = is_maker)
    })
}

#' @export
coin_trades <- function(symbol = NULL, limit = 100){

  symbol %>%
    check_symbol(limit = limit) %>%
    purrr::map_dfr(~{
      bi_get(path = "trades", data = .x) %>%
        purrr::map_dfr(tibble::as_tibble) %>%
        dplyr::mutate(symbol = .x$symbol)
    }) %>%
    janitor::clean_names() %>%
    dplyr::mutate(timestamp = lubridate::now(tz = "EST"))

}

#' @export
generic_symbol_endpoint <- function(symbol = NULL, endpoint){
  symbol %>%
    check_symbol %>%
    purrr::map_dfr(~{
      bi_get(path = endpoint, data = .x, version = "v3") %>%
        tibble::as_tibble()
    }) %>%
    janitor::clean_names() %>%
    dplyr::mutate(timestamp = lubridate::now(tz = "EST"))
}

#' @export
coin_average_price <- function(symbol = NULL) generic_symbol_endpoint(symbol = symbol, endpoint = "avgPrice")

#' @export
coin_ticker <- function(symbol = NULL) generic_symbol_endpoint(symbol = symbol, endpoint = "ticker/24hr")

#' @export
coin_symbol_ticker <- function(symbol = NULL) generic_symbol_endpoint(symbol = symbol, endpoint = "ticker/price")

#' @export
coin_book_ticker <- function(symbol = NULL) generic_symbol_endpoint(symbol = symbol, endpoint = "ticker/bookTicker")

#' @export
recent_trades <- function(symbol, limit = 500){
  bi_get(path = "trades", data = list(symbol = symbol, limit = limit)) %>%
    purrr::map_dfr(tibble::as_tibble) %>%
    janitor::clean_names() %>%
    dplyr::mutate(symbol = symbol)
}

#' @export
historical_trades <- function(symbol, limit = 500, last_id = NULL){
  data <- list(symbol = symbol, limit = limit)
  if(!is.null(last_id)) data$fromId <- last_id

  bi_get(path = "historicalTrades", data = data) %>%
    purrr::map_dfr(tibble::as_tibble) %>%
    janitor::clean_names() %>%
    dplyr::mutate(symbol = symbol)
}

