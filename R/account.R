#' @export
register_account <- function(api_key, api_secret, keep_registered = F){
  if(keep_registered){
    set_renv("BIN_API" = api_key)
    set_renv("BIN_SK" = api_secret)
  } else {
    Sys.setenv("BIN_API" = api_key)
    Sys.setenv("BIN_SK" = api_secret)
  }
}

#' @export
api_key <- function() Sys.getenv("BIN_API")
#' @export
api_secret <- function() Sys.getenv("BIN_SK")

#' @export
account_headers <- function(){
  c(accept = "application/json",
    `User-Agent` = 'binance/python',
    `X-MBX-APIKEY` = api_key())
}

#' @export
coin_account <- function(){
  bi_get(type = "api", signed = T, path = "account") %>%
    purrr::imap_dfc(~{
      if(length(.x) == 0) return(NULL)
      if(.y == "balances") return(tibble::tibble(balances = list(.x %>% purrr::map_dfr(tibble::as_tibble))))

      tibble::tibble(a = .x) %>% purrr::set_names(.y)
    }) %>%
    janitor::clean_names()
}

#' @export
coin_assets <- function(assets = NULL){
  out <- coin_account() %>%
    dplyr::pull(balances) %>%
    .[[1]] %>%
    dplyr::mutate(free = as.numeric(free),
                  locked = as.numeric(locked)) %>%
    dplyr::filter(as.numeric(free) > 0)

  if(!is.null(assets)){
    out <- out %>% filter(asset %in% assets)
  }

  out

}

#' @export
coin_wallets <- function(accounts){

  prices <- coinr::coin_prices() %>%
    dplyr::bind_rows(tibble::tibble(symbol = "USDTUSDT", price = 1))

  coinr::coin_assets() %>%
    dplyr::mutate(symbol = paste0(asset, "USDT")) %>%
    dplyr::left_join(prices, "symbol") %>%
    dplyr::filter(!is.na(price)) %>%
    dplyr::mutate(value = free * price) %>%
    dplyr::select(-symbol)

}

#' @export
coin_value_wallets <- function(account){
  coin_wallets() %>%
    dplyr::summarise(value = sum(value) + sum(locked)) %>%
    dplyr::pull(value)
}
