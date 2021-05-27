
#' @export
bi_get <- function(type = "api", path = "ping", signed = FALSE, version = bi_params$public_api_version, ...){

  url <- bi_create_url(path = path, type = type, signed = signed, version = version)

  url %>%
    bi_request(method = "get", signed = signed, ...)
}

#' @export
bi_post <- function(type = "api", path = "ping", signed = FALSE, version = bi_params$public_api_version, ...){

  url <- bi_create_url(path = path, type = type, signed = signed, version = version)

  url %>%
    # some endpoints have an addition true parameter
    bi_request(method = "post", signed = signed, ...)
}

#' @export
bi_delete <- function(type = "api", path = "ping", signed = FALSE, version = bi_params$public_api_version, ...){

  url <- bi_create_url(path = path, type = type, signed = signed, version = version)

  url %>%
    # some endpoints have an addition true parameter
    bi_request(method = "delete", signed = signed, ...)
}


#' @export
bi_create_url <- function(path, type = "api", signed = F, version = bi_params$public_api_version){

  params <- switch(
    type,
    api = list(url = bi_params$api_url, version = ifelse(signed, bi_params$private_api_version, version)),
    future = list(url = bi_params$futures_url, version = bi_params$futures_api_version),
    website = list(url = bi_params$website_url, version = NULL),
    margin = list(url = bi_params$margin_api_url, version = bi_params$margin_api_version),
    withdraw = list(url = bi_params$withraw_api_url , version = bi_params$withdraw_api_version),
  )

  paste0(params$url, "/", ifelse(is.null(params$version), "", paste0(params$version, "/")), path)

}

