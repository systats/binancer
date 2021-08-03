#' @export
create_order <- function(.tbl,
                         time_in_force = NULL, price = NULL, stop_price = NULL,
                         quote_order_quantity = NULL, new_client_order_id = NULL,
                         iceberg_quantity = NULL, new_order_response_type = "RESULT", receiving_window = NULL,
                         test = T){

  # Required for any order_type
  data <- list(
    symbol = .tbl$currency,
    side = .tbl$action,
    type = .tbl$order_type,
    quantity = .tbl$final_quantity
  )

  if(stringr::str_detect(.tbl$order_type, "LIMIT")){
    if(is.null(time_in_force) & .tbl$order_type != "LIMIT_MAKER"){
      warning(glue::glue("Argument time_in_force is required for orders {.tbl$order_type}"))
    } else {
      if(.tbl$order_type != "LIMIT_MAKER") data$timeInForce <- time_in_force
    }
    if(is.null(price)){
      warning(glue::glue("Argument price is required for orders {.tbl$order_type}"))
    } else {
      data$price <- round(price, 8)
    }
  }

  if(stringr::str_detect(.tbl$order_type, "(STOP_LOSS)|(TAKE_PROFIT)")){
    if(is.null(stop_price)){
      warning(glue::glue("Argument stop_price is required for orders {.tbl$order_type}"))
    } else {
      data$stopPrice <- round(stop_price, 8)
    }
  }

  res <- bi_post(path = "order/test", signed = T, data = data)
  if(test) return(.tbl %>% dplyr::mutate(test = list(res)))

  if(length(res) == 0){
    out <- bi_post(path = "order", signed = T, data = data)
  } else {
    print(res)
    stop("Something went wrong while testing")
  }
  return(.tbl %>% dplyr::mutate(resp = list(out)))

}

#' @export
coin_ftrade <- function(exchanges_all = NULL, lot_size = NULL, taking, giving, taking_qt = NULL, giving_qt = NULL,
                        symbols = NULL,
                        limit_price = NULL){

  if(is.null(symbols)) symbols <- symbols()$symbol

  # tictoc::tic()
  market <- paste0(taking, giving)
  rev <- market %in% symbols
  if(!rev) market <- paste0(giving, taking)

  data <- list(
    symbol = market,
    side = if(rev) "BUY" else "SELL",
    type = if(is.null(limit_price)) "MARKET" else "LIMIT"
  )
  # tictoc::toc()
  # tictoc::tic()


  if(rev){
    if(is.null(giving_qt)){
      # print(1)
      if(is.null(lot_size)){
        lot_size <- as.numeric(dplyr::filter(exchanges_all, symbol == market)[["lot_siz_estep_size"]])
      }
      data$quantity <- lot_size * floor(as.numeric(as.character(taking_qt / lot_size)))
    } else {
      # print(2)
      data$quoteOrderQty <- giving_qt
    }
  } else {
    if(is.null(giving_qt)){
      # print(3)
      data$quoteOrderQty <- taking_qt
    } else {
      # print(4)
      if(is.null(lot_size)){
        lot_size <- as.numeric(dplyr::filter(exchanges_all, symbol == market)[["lot_siz_estep_size"]])
      }
      data$quantity <- lot_size * floor(as.numeric(as.character(giving_qt / lot_size)))
    }
  }

  # tictoc::toc()
  # tictoc::tic()

  if(!is.null(limit_price)){
    data$timeInForce <- "GTC"
    data$price <- round(limit_price, 8)
    if(rlang::has_name(data, "quoteOrderQty")){
      if(is.null(lot_size)){
        lot_size <- as.numeric(dplyr::filter(exchanges_all, symbol == market)[["lot_siz_estep_size"]])
      }
      data$quantity <- lot_size * floor(as.numeric(as.character(data$quoteOrderQty / limit_price / lot_size)))
      data$quoteOrderQty <- NULL
    }
  }

  # tictoc::toc()
  # tictoc::tic()
  post_request_fast(path = "order", data = data)
  # tictoc::toc()

}

#' @export
coin_trade <- function(exchanges_all, taking, giving, taking_qt = NULL, giving_qt = NULL, test = T, verbose = F){

  if(is.null(taking_qt)) out <- tibble::tibble(taking = taking, giving = giving, giving_qt = giving_qt)
  if(is.null(giving_qt)) out <- tibble::tibble(taking = taking, giving = giving, taking_qt = taking_qt)

  order <- out %>%
    check_market(verbose = verbose, symbols = exchanges_all$symbol) %>%
    add_price_currency(verbose = verbose) %>%
    shape_quantities(verbose = verbose) %>%
    correc_quantity(exchanges_all, verbose = verbose)  %>%
    dplyr::mutate(order_type = "Market")

  if(verbose) dplyr::glimpse(order)

  order %>%
    create_order(test = test) %>%
    message_order(verbose = verbose)

}
