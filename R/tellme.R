# #
# #
# library(data.table)
#
# get_status_df <- function(path_to_sp500){
#   rbindlist(lapply(list.files(path_to_sp500, full.names = T), function(x){
#     t <- readRDS(x)
#     corona <- as.Date('2020-02-20')
#     last_tarade_before_corona <- t$date[which(abs(t$date- corona ) == min(abs(t$date - corona )))]
#     rise_last_year_before_corona <- ((t[t$date==last_tarade_before_corona, ]$close /  t[t$date==t$date[which(abs(t$date- (corona- 365) ) == min(abs(t$date - (corona- 365) )))], ]$close)-1)*100
#
#     max_before_corona <- max(t[t$date>(corona-10)&t$date<(corona+10) ]$high)
#
#
#     fall_from_corona <- (1 - (min(t[date>=corona,]$low) / max_before_corona)) *100
#     current_corona_fall <- (1 - (  tail(t$close,1)  / max_before_corona)) *100
#     rise_from_corona_min <-  (tail(t$close,1) / min(t[date>=corona,]$low) -1)*100
#
#     did_it_fall_below_200 <-sum(t[date>=corona,]$diff_200_ma_value < -3 ) >0
#     adat <-cbind(data.table( 'ticker' = strsplit(tail(strsplit(x, '/')[[1]], 1), '.', fixed = T)[[1]][1], 'rise_last_year_before_corona'= rise_last_year_before_corona,
#                              'fall_from_corona' = fall_from_corona, 'current_corona_fall'=current_corona_fall, 'diff_to_max_fall' = fall_from_corona- current_corona_fall, 'rise_from_corona_min'=rise_from_corona_min,
#                              'did_it_fall_below_200' = did_it_fall_below_200),
#                  tail(t,1))
#
#     return(adat)
#
#   }))
# }
# status_df <- get_status_df('/home/mihaly/sp500')
#
# get_ggplot_of_ticker_to_slack('ARNC',slack_token = slack_token, all_time_low_slack_id)
#
#
#
#
#
# #get_ggplot_of_ticker_to_slack('IBM',slack_token = slack_token, all_time_low_slack_id)
#
# # library(getrich)
# # library(ggplot2)
# # library(plotly)
# # library(rbokeh)
# # library(httr)
# #
# # post_to_chanel <- function(ticker) {
# #
# #   p<-  get_info_plot(ticker, Sys.Date()-365)
# #   widget2png(p, '/home/mihaly/R_codes/getrich/pp.png')
# #
# #
# #   POST(url = "https://slack.com/api/chat.postMessage",
# #        body = list(token = slack_token, channel = 'all_time_low', text = 'hello'))
# #
# #   ftmp <- 'file.png'
# #   POST(url="https://slack.com/api/files.upload",
# #        add_headers(`Content-Type`="multipart/form-data"),
# #        body=list(file=upload_file(ftmp),
# #                  token=slack_token, channels='C0142J4ET9A'))
# #
# #
# # }
# #
# #
# # orca(p, file = paste0(ticker,".png"))
# # my_ticker <- "TSLA"
# #
# # p <- plot_ly(x = 1:10, y = 1:10, color = 1:10)
# # orca(p, "plot.svg")
# #
# # library(rbokeh)
# # widget2png(p, 'file.png', timeout = 500)
# #
# # png(file="saving_plot2.png",
# #     width=600, height=350)
# # plot(p)
# #
# # dev.off()
# #
# #
# # #source('/home/mihaly/keys.R')
#
#
#
#
#
#
#
#
#
#
#
#
# #getrich::get_tradingview_data_from_json_string('{"filter":[{"left":"Perf.1M","operation":"nempty"}],"options":{"lang":"en"},"symbols":{"query":{"types":[]},"tickers":[],"groups":[{"type":"index","values":["SP:SPX"]}]},"columns":["logoid","name","change|1","change|5","change|15","change|60","change|240","change","Perf.W","Perf.1M","Perf.3M","Perf.6M","Perf.YTD","Perf.Y","beta_1_year","Volatility.D","description","name","type","subtype","update_mode"],"sort":{"sortBy":"Perf.1M","sortOrder":"asc"},"range":[0,650]}')
#
# #
# #
# #
# #
# #
# # "https://api.slack.com/methods/channels.list/test"
# #
# # res <- POST(url="https://slack.com/api/files.upload",
# #             add_headers(`Content-Type`="multipart/form-data"),
# #             body=list(file=upload_file(ftmp),
# #                       token='xoxp-876918927600-878779152743-876473795092-421c9d0cdebba90766f23d2f6ec066f0', channels='C0142J4ET9A'))
# #
# #
# #
# # library(plotly)
# # library(rvest)
# # library(jsonlite)
# # library(getrich)
# # library(data.table)
# # library(TTR)
# # library(pracma)
# # library(rtsdata)
# # get_summary_of_ticker <- function(ticker){
# #   #  ticker<- 'TSLA'
# #   t <- read_html(paste0('https://www.tradingview.com/symbols/', ticker, '/'))
# #   return(fromJSON(
# #     t %>%
# #       html_nodes(xpath = "//script[@type='application/ld+json']") %>% '[['(3) %>%
# #       html_text()
# #   ))
# # }
# # get_info_plot  <- function(my_ticker,show_date) {
# #   my_info <- get_summary_of_ticker(my_ticker)
# #   show_date <- as.Date(show_date)-365
# #   t <- get_one_ticker(ticker = my_ticker, start_date = show_date, end_date = Sys.Date())
# #   t <- t[date>(as.Date(show_date)+365) ,]
# #   t$fall_from_max <- ((t$low / max(t$high)) -1 )*100
# #   t$fall_from_close <- ((t$close / max(t$high)) -1 )*100
# #   t$fall_from_high <- ((t$high / max(t$high)) -1 )*100
# #
# #   fig <-plot_ly(t, x=~date, type="candlestick",
# #                 open = ~open, close = ~close,
# #                 high = ~high, low = ~low) %>%
# #     layout(title = "Basic Candlestick Chart", showlegend = FALSE,  xaxis = list(rangeslider = list(visible = F))) %>%
# #     add_lines(x=~date,y=~ma_200_value, name='200 daily MA') %>%
# #     add_lines(x=~date,y=~ma_50_value, name='50 daily MA')
# #
# #   #fig
# #
# #   fall_plot <-
# #     plot_ly(t, x=~date,y=~fall_from_max,  type = 'scatter', mode = 'lines', name = 'min')  %>%
# #     add_lines(x=~date,y=~fall_from_close, name='close') %>%
# #     add_lines(x=~date,y=~fall_from_high, name='high')  %>%
# #     layout(title = "", showlegend = F,  xaxis =list(title=""), yaxis =list(title="Fall from max (%)") )
# #
# #
# #   rsi_plot <-
# #     plot_ly(t, x=~date,y=~rsi,  type = 'scatter', mode = 'lines', name = 'min') %>%
# #     add_trace(x=~date,y=30, line = list(color = 'red', width = 4, dash = 'dash') ) %>%
# #     add_trace(x=~date,y=70, line = list(color = 'red', width = 4, dash = 'dash')) %>%
# #     layout(title = "", showlegend = F,  xaxis =list(title=""), yaxis =list(title="RSI") )
# #
# #   all_p<- subplot(list(fig, rsi_plot, fall_plot ), nrows = 3, shareX = T, shareY = F, margin = 0.01, heights = c(0.5, 0.25, 0.25))
# #   return(list(my_plot = all_p, 'name'= my_info$name, 'desc' = my_info$description, 'ticker' = my_info$tickerSymbol))
# #
# # }
# # get_info_plot('TSLA', Sys.Date()-720)
