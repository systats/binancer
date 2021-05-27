#' @export
parse_symbols <- function(.x){

  .x %>%
    purrr::imap_dfc(~{
      if(.y == "orderTypes") return(tibble::tibble(ot = paste0(.x, collapse = "_")))
      if(.y == "permissions") return(tibble::tibble(permissions = paste0(.x, collapse = "_")))
      if(.y == "filters") return(
        .x %>%
          purrr::map_dfc(~{
            type <- .x$filterType
            .x %>%
              tibble::as_tibble()  %>%
              dplyr::select(-filterType) %>%
              dplyr::rename_all(~paste0(type, .x))
          }))

      tibble::tibble(a = .x) %>% purrr::set_names(.y)
    }) %>%
    janitor::clean_names()

}

#' @export
parse_exchange_info <- function(ex, symbol = NULL){

  if(is.null(symbol)){
    rt <- tibble::tibble(symbols = list(ex$symbols %>% furrr::future_map_dfr(coinr::parse_symbols, .progress = T)))
  } else {
    rt <- tibble::tibble(symbols = list(ex$symbols %>% purrr::map_dfr(~{
      if(stringr::str_detect(.x$symbol, symbol)) coinr::parse_symbols(.x) else tibble::tibble()
    })))
  }

  ex %>%
    purrr::imap_dfc(~{
      # if(.y == "symbols") return(tibble(symbols = list(.x %>% future_map_dfr(parse_symbols, .progress = T))))
      if(length(.x) == 0) return(NULL)
      if(.y == "symbols") return(rt)
      if(.y == "rateLimits") return(tibble::tibble(rate_limits = list(.x %>% purrr::map_dfr(tibble::as_tibble))))

      tibble::tibble(a = .x) %>% purrr::set_names(.y)
    }) %>%
    janitor::clean_names()

}

#' @export
parse_book <- function(book){
  book %>%
    purrr::imap_dfc(~{
      name <- .y
      if(is.list(.x)){
        .x %>%
          purrr::map_dfr(~{
            names(.x) <- c("price", "qty")
            tibble::as_tibble(.x) %>%
              dplyr::rename_all(~paste(name, .x,sep = "_"))
          })
      } else{
        tibble::tibble(.x) %>% purrr::set_names(name)
      }
    })
}
