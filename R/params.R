
#' @export
tld <- "com"

bi_params <- list(

  api_url = glue::glue('https://api.binance.{tld}/api'),
  withraw_api_url = glue::glue('https://api.binance.{tld}/wapi'),
  margin_api_url = glue::glue('https://api.binance.{}/sapi'),
  website_url = glue::glue('https://www.binance.{tld}'),
  futures_url = glue::glue('https://fapi.binance.{tld}/fapi'),
  public_api_version = 'v1',
  private_api_version = 'v3',
  withdraw_api_version = 'v3',
  margin_api_version = 'v1',
  futures_api_version = 'v1',

  symbole_type_spot = 'SPOT',

  os_enw = 'NEW',
  os_partially_filled = 'PARTIALLY_FILLED',
  os_filled = 'FILLED',
  os_cancelled = 'CANCELED',
  os_pending_cancel = 'PENDING_CANCEL',
  os_rejected = 'REJECTED',
  os_expired = 'EXPIRED',

  kline_ineterval_1m = '1m',
  kline_ineterval_3m = '3m',
  kline_ineterval_5m = '5m',
  kline_ineterval_15m = '15m',
  kline_ineterval_30m = '30m',
  kline_ineterval_1h = '1h',
  kline_ineterval_2h = '2h',
  kline_ineterval_4h = '4h',
  kline_ineterval_6h = '6h',
  kline_ineterval_8h = '8h',
  kline_ineterval_12h = '12h',
  kline_ineterval_1d = '1d',
  kline_ineterval_3d = '3d',
  kline_ineterval_1w = '1w',
  kline_ineterval_1m = '1M',

  ot_resp_ACK = 'ACK',
  ot_resp_RESULT = 'RESULT',
  ot_resp_FULL = 'FULL',

  # For accessing the data returned by Client.aggregate_trades().
  agg_id = 'a',
  agg_price = 'p',
  agg_quantity = 'q',
  agg_ft_id = 'f',
  agg_lt_id = 'l',
  agg_time = 'T',
  agg_buyer_makes = 'm',
  agg_best_match = 'M'

)
