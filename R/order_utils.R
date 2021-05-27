

#' @export
check_market <- function(.tbl, verbose = T){

  out <- .tbl %>%
    dplyr::mutate(currency = dplyr::case_when(
      paste0(giving, taking) %in% symbols$symbol ~ paste0(giving, taking),
      paste0(taking, giving) %in% symbols$symbol ~ paste0(taking, giving),
      T ~ NA_character_
    ),
    reverse = stringr::str_detect(currency, paste0("^", giving)),
    action = ifelse(reverse, "SELL", "BUY"))

  if(verbose){
    if(is.na(out$currency)){

      stop("No market linking giving currency and taking currency.")

    } else {
      cli::cli_alert_info(glue::glue("Market: {out$currency}"))
    }
    if(out$reverse){
      cli::cli_alert_info(glue::glue("{stringr::str_to_title(out$action)}ing {out$giving} and receiving {out$taking}"))
    } else {
      cli::cli_alert_info(glue::glue("{stringr::str_to_title(out$action)}ing {out$taking} and paying in {out$giving}"))
    }
  }

  return(out)
}

#' @export
add_price_currency <- function(.tbl, verbose = T){

  out <- .tbl %>%
    dplyr::mutate(price = currency %>%
                    coin_prices %>%
                    .$price %>%
                    as.numeric)

  if(verbose){
    if(out$reverse){
      cli::cli_alert_info(glue::glue("Price: {out$price} {out$taking} for 1 {out$giving}"))
    } else {
      cli::cli_alert_info(glue::glue("Price: {out$price} {out$giving} for 1 {out$taking}"))
    }
  }

  return(out)
}

#' @export
shape_quantities <- function(.tbl, verbose = T){

  if(!is.null(.tbl[["giving_qt"]])){

    out <- .tbl %>%
      dplyr::mutate(taking_qt = ifelse(reverse, giving_qt*price, giving_qt/price))

  } else if(!is.null(.tbl[["taking_qt"]])){

    out <- .tbl %>%
      dplyr::mutate(giving_qt = ifelse(reverse, taking_qt/price, taking_qt*price))

  } else {

    stop("You need to specify one of {.tbl$giving_qt} or {.tbl$taking_qt}")

  }

  if(verbose){
    if(out$reverse){
      cli::cli_alert_info(glue::glue("Receiving {out$taking_qt} {out$taking} for {out$taking_qt/out$price} {out$giving}"))
    } else {
      cli::cli_alert_info(glue::glue("Giving {out$taking_qt*out$price} {out$giving} for {out$taking_qt} {out$taking}"))
    }
  }

  return(out)

}

#' @export
correc_quantity <- function(.tbl,  exchanges_all, verbose = T){

  ex <- exchanges_all %>%
    dplyr::filter(stringr::str_detect(symbol, unique(.tbl$currency)))

  min <- as.numeric(ex$lot_siz_emin_qty[1])
  step <- as.numeric(ex$lot_siz_estep_size[1])

  if(verbose){
    if(.tbl$reverse){
      cli::cli_alert_info(glue::glue("Min quantity is {min} {.tbl$giving}. It must be imcremented in steps of {step} {.tbl$giving}"))
    } else {
      cli::cli_alert_info(glue::glue("Min quantity is {min} {.tbl$taking}. It must be imcremented in steps of {step} {.tbl$taking}"))
    }
  }

  out <- .tbl %>%
    dplyr::mutate(final_quantity = min + floor(as.numeric(as.character((ifelse(reverse, giving_qt, taking_qt) - min) / step))) * step)

  if(out$final_quantity != out$taking_qt){
    if(verbose){
      if(out$reverse){
      cli::cli_alert_info(glue::glue("Corrected quantities: receiving {out$final_quantity*out$price} {out$taking} for {out$final_quantity} {out$giving}"))
      } else {
      cli::cli_alert_info(glue::glue("Corrected quantities: giving {out$final_quantity*out$price} {out$giving} for {out$final_quantity} {out$taking}"))
      }
    }
  }

  # if(verbose) cli::cli_alert_info("Min notional is {ex$min_notiona_lmin_notional}")

  return(out)
}

#' @export
message_order <- function(.tbl, verbose = T){

  if("test" %in% names(.tbl)){
    is_test <- T
    test <- "[ test ]"
    res <- .tbl$test[[1]]
  } else {
    is_test <- F
    test <- "[ real ]"
    res <- .tbl$resp[[1]]
  }

  if(length(res) == 0){res <- list(status = "success")}

  if(length(setdiff(names(res), c("msg", "code"))) ==0){
    msg <- glue::glue("[ {Sys.time()} - {.tbl$currency} ] Code: {res$code}\t{res$msg}")
    if(verbose) cli::cli_alert_danger(msg)
  } else {
    if(.tbl$action == "BUY"){
      msg <- glue::glue("[ {Sys.time()} - {.tbl$currency} ] {test} Buying {.tbl$final_quantity} {.tbl$taking} for a total of {.tbl$final_quantity*.tbl$price} {.tbl$giving}")
    } else {
      msg <- glue::glue("[ {Sys.time()} - {.tbl$currency} ] {test} Selling {.tbl$final_quantity} {.tbl$giving} for a total of {.tbl$final_quantity*.tbl$price} {.tbl$taking}")

    }

    if(verbose) cli::cli_alert_success(msg)
  }

  return(.tbl)
}
