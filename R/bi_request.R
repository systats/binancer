#' @export
post_request_fast <- function(path, data){

  url <- paste(bi_params$api_url, bi_params$private_api_version, path, sep = "/")

  data$timestamp <- round(as.numeric(lubridate::now("UTC"))*1000)
  data$signature <- generate_signature(data = data)

  res <- httr::POST(url,
                    httr::add_headers(.headers = account_headers()),
                    body = order_params(data), encode = "form") %>%
    httr::content()
  return(res)
}

#' @export
bi_request <- function(url, method, signed, force_params = FALSE, requests_params = NULL, ...){
  timeout <- 10

  dots <- list(...)

  #dots <- list()
  #dots <- list(data = data)
  dots$a <- ""
  dots$requests_params <- requests_params

  has_data <- "data" %in% names(dots)
  if(!has_data){
    dots$data <- tibble::tibble(a = "")
  }

  # Merge request param if request param in dots$data
  # remove it form dots$data

  if(signed){
    dots$data$timestamp <- round(as.numeric(lubridate::now("UTC"))*1000)
    if(!has_data){
      dots$data$a <- NULL
    }
    # print(timestamp)
    dots$data$signature <- generate_signature(data = dots$data)
    # print(dots$data$signature)
  }

  if(!has_data){
    if(signed){
      dots$data <- dots$data %>%
        order_params() %>%
        #remove all null arguments
        purrr::compact()
    }else {
      dots$data <- NULL
    }
  } else {
    dots$data <- dots$data %>%
      order_params() %>%
      #remove all null arguments
      purrr::compact()
  }

  # print(force_params)

  if(!is.null(dots$data) & (method == "get"| force_params)){
    dots$params <- collapse_query(data = dots$data)
  }

  if(identical(dots, list(a = ""))){
    dots <- NULL
  }

  if(api_key() == ""){
    stop("No user found. Please register a coinr-user using 'register_account(api_key = '<your_api_key>', api_secret = '<your_api_secret>')'")
  }

  if(method == "get"){

    if(!is.null(dots)){
      if(stringr::str_detect(url, "\\?"))stop("Url already has query parameters")
      url <- paste0(url, dots$params)
    }

    # print(url)
    res <- httr::GET(url, httr::add_headers(.headers = account_headers())) %>%
      httr::content()

  } else if( method == "post"){
    # print(dots$data)
    # print(account)
    res <- httr::POST(url, httr::add_headers(.headers = account_headers()), body = dots$data, encode = "form") %>%
      httr::content()
  } else if( method == "delete"){
    # print(dots$data)
    # print(account)
    res <- httr::DELETE(url, httr::add_headers(.headers = account_headers()), body = dots$data, encode = "form") %>%
      httr::content()
  }

  return(res)
}

#' @export
delete_order <- function(symbol, order_id = NULL){
  del_data <- list(symbol = symbol)
  if(!is.null(order_id)){
    del_data$orderId <- order_id
    out <- bi_delete(path = "order", signed = T, data = del_data)
  } else {
    out <- bi_delete(path = "openOrders", signed = T, data = del_data)
  }
  if(length(setdiff(c("code", "msg"), names(out))) == 0) stop(glue::glue("Order could not be deleted.\n{out$code}/{out$msg}"))

  return(dplyr::mutate(tibble::as_tibble(del_data), type = "delete", resp = list(out)))
}



