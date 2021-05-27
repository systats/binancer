
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Binancer

<!-- badges: start -->
<!-- badges: end -->

Binancer provides access to Binance api.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("benjaminguinaudeau/binancer")
```

## How to trade

``` r
# This provides binancer credentials information to place orders and retrieve order information
register_account(
  api_key = "<your_api_key>",
  api_secret = "<your_api_secret>", 
  keep_registered = F #If true, the api_keys will be written in your .Renviron and load at each new session
)
```

### Get the rules for each market

``` r
exchanges_all <- coin_exchange_info() %>% #Can take some time
  dplyr::glimpse()
# readr::write_rds(exchanges_all, "data/exchanges_all.rds")
# exchanges_all <- readr::read_rds("data/exchanges_all.rds")
```

### Do some trades

``` r
# Buy 10 BTC
coin_trade(exchanges_all, taking = "BNB", giving = "USDT", taking_qt = .1, test = T, verbose = T)
# Buy BTC for 15 BUSD
coin_trade(exchanges_all, taking = "BNB", giving = "USDT", giving_qt = 15, test = T, verbose = T)

# Sell BTC for 15 BUSD
coin_trade(exchanges_all, taking = "USDT", giving = "BNB", taking_qt = 15, test = T, verbose = T)

# Sell 10 BTC 
coin_trade(exchanges_all, taking = "USDT", giving = "BNB", giving_qt = .125, test = T, verbose = T)
```

## Check your assets

``` r
# Return how much of each coin is detained
coin_wallets() 
# Equity of all portfolios
coin_value_wallets() 
```

## Trade history

``` r
coin_all_orders(symbol = "BTCUSDT", limit = 5) # 5 last BTCUSDT orders
coin_all_orders(symbol = c("BTCUSDT", "ADAUSDT"), limit = 5)
coin_all_orders(symbol = c("BTCUSDT", "DENTUSDT"), limit = 5)
```

## Other function

``` r
# What is the current time of Binance Server
coin_server_time()
# Is everything normal
coin_system_status()
```

### Prices

``` r
coin_prices()
coin_prices(symbol = "^USDT")
```

### Orders book

``` r
coin_book()
coin_book(symbol = "BTCUSDT")
coin_book(symbol = "BTCUSDT", limit = 1000)
coin_book(symbol = "^USDT", limit = 10)
```

### Get trades

``` r
coin_trades(symbol = "BTCUSDT")
coin_trades(symbol = "BTCUSDT", limit = 1000)
coin_trades(symbol = "^USDT", limit = 10)
```

### Get average price

``` r
coin_average_price(symbol = "BTCUSDT")
coin_average_price(symbol = "^USDT")
```

### Ticker

``` r
coin_ticker(symbol = "BTCUSDT")
coin_ticker(symbol = "^USDT")

# coin_symbol_ticker() error
coin_symbol_ticker(symbol = "BTCUSDT")
coin_symbol_ticker(symbol = "^USDT")

# coin_book_ticker() error
coin_book_ticker(symbol = "BTCUSDT")
coin_book_ticker(symbol = "^USDT")
```
