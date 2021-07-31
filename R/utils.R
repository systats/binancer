#' @export
from_unix <- function(x) as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC")

#' @export
collapse_query <- function(data){
    data %>%
        purrr::imap_chr(~paste0(.y, "=", .x)) %>%
        paste(collapse = "&") %>%
        paste0("?", .)
}

#' @export
generate_signature <- function(data){

    query_string = data %>%
        order_params %>%
        collapse_query

    digest::hmac(enc2utf8(api_secret()), enc2utf8(stringr::str_remove(query_string, "^\\?")), "sha256")

}

#' @export
order_params <- function(data){

    signature <- data[["signature"]]
    data$signature <- NULL
    params <- data[order(names(data))]

    if(!is.null(signature)){
        params$signature <- signature
    }

    return(params)

}

#' @export
symbols <- function(){
    readr::read_rds(system.file("symbols.rds", package = "binancer"))
}

#' @export
update_symbols <- function(){
    readr::write_rds(coin_prices()[,"symbol"], system.file("symbols.rds", package = "binancer"))
}

#' @export
check_symbol <- function(symbol, limit = NULL, required = T){

    if(required & is.null(symbol)){
        stop("Symbol must be specified")
    }

    if(!symbol %in% symbols()){
        update_symbols()
    }

    symbols() %>%
        dplyr::filter(stringr::str_detect(symbol, !!symbol)) %>%
        dplyr::select(symbol) %>%
        dplyr::mutate(limit = !!limit) %>%
        split(1:nrow(.))
}
