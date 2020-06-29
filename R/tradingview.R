#' Return the sector industry data
#' @export
get_sector_ind <- function() {
  t <-   get_tradingview_data_from_json_string('{"filter":[{"left":"market_cap_basic","operation":"nempty"},{"left":"type","operation":"in_range","right":["stock","dr","fund"]},{"left":"subtype","operation":"in_range","right":["common","","etf","unit","mutual","money","reit","trust"]},{"left":"exchange","operation":"in_range","right":["AMEX","NASDAQ","NYSE"]}],"options":{"lang":"en"},"symbols":{"query":{"types":[]},"tickers":[]},"columns":["name","close","change","change_abs","Recommend.All","volume","market_cap_basic","price_earnings_ttm","earnings_per_share_basic_ttm","number_of_employees","sector","industry","description","name","type","subtype","update_mode","pricescale","minmov","fractional","minmove2"],"sort":{"sortBy":"market_cap_basic","sortOrder":"desc"},"range":[0,5000]}')
  t <- t[,c('name', 'sector', 'industry', 'market_cap_basic')]
  t$market_cap_basic <- as.numeric(t$market_cap_basic)/1000000
  return(t)
}

#' Return the sector industry data
#' @export
#' @param my_json_string json string from inspect view at the bottom
#' @import data.table
#' @import jsonlite
#' @import httr
get_tradingview_data_from_json_string <- function(my_json_string) {
  headers = c(
    `authority` = 'scanner.tradingview.com',
    `accept` = 'text/plain, */*; q=0.01',
    `origin` = 'https://www.tradingview.com',
    `user-agent` = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.108 Safari/537.36',
    `content-type` = 'application/x-www-form-urlencoded; charset=UTF-8',
    `sec-fetch-site` = 'same-site',
    `sec-fetch-mode` = 'cors',
    `referer` = 'https://www.tradingview.com/',
    `accept-encoding` = 'gzip, deflate, br',
    `accept-language` = 'hu-HU,hu;q=0.9,en-US;q=0.8,en;q=0.7'
  )


  res <- httr::POST(url = 'https://scanner.tradingview.com/america/scan', httr::add_headers(.headers=headers), body = my_json_string)

  t <- fromJSON(content(res, 'text'))
  my_df_data <-
    rbindlist(lapply(t$data$d, function(x){
      data.frame(t(data.frame(x)), stringsAsFactors = F)
    }))

  my_names <- strsplit(strsplit( strsplit(gsub('\"', '', my_json_string, fixed = T), 'columns:[', fixed = T)[[1]][2], '],sort',  fixed = T )[[1]][1], ',', fixed = T)[[1]]

  names(my_df_data) <- my_names
  final_data <- cbind( data.table('exchange' = sapply(strsplit(t$data$s, ':'), '[[', 1)),  my_df_data)
  return(final_data)
}

#' Return the S&P500 tickers
#' @export
#' @param just_ticker if it is True, it will return justt with the ticker ids
get_sp500 <- function(just_ticker=T) {
  adat <- get_tradingview_data_from_json_string('{"filter":[{"left":"name","operation":"nempty"}],"options":{"lang":"en"},"symbols":{"query":{"types":[]},"tickers":[],"groups":[{"type":"index","values":["SP:SPX"]}]},"columns":["name","close","change","change_abs","Recommend.All","volume","market_cap_basic","price_earnings_ttm","earnings_per_share_basic_ttm","number_of_employees","sector","description","name","type","subtype","update_mode","pricescale","minmov","fractional","minmove2"],"sort":{"sortBy":"name","sortOrder":"asc"},"range":[0,600]}')
  if (just_ticker) {
    return(adat$name)
  }else{
    return(adat)
  }

}


#' Return all the ticker from tradingview with performance data
#' @export
get_performance_table <- function() {
  adat <- get_tradingview_data_from_json_string('{"filter":[{"left":"market_cap_basic","operation":"nempty"},{"left":"type","operation":"in_range","right":["stock","dr","fund"]},{"left":"subtype","operation":"in_range","right":["common","","etf","unit","mutual","money","reit","trust"]},{"left":"exchange","operation":"in_range","right":["AMEX","NASDAQ","NYSE"]}],"options":{"lang":"en"},"symbols":{"query":{"types":[]},"tickers":[]},"columns":["logoid","name","change|1","change|5","change|15","change|60","change|240","change","Perf.W","Perf.1M","Perf.3M","Perf.6M","Perf.YTD","Perf.Y","beta_1_year","Volatility.D","RSI","number_of_employees","market_cap_basic","sector","industry","Low.6M","High.All","Low.All","price_52_week_high","price_earnings_ttm","close","description","name","type","subtype","update_mode","RSI","RSI[1]","pricescale","minmov","fractional","minmove2"],"sort":{"sortBy":"market_cap_basic","sortOrder":"desc"},"range":[0,6550]}')
  return(adat)
}


