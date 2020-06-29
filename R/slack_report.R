#' Get info of a ticker from tradingview
#' @export
#' @param Ticker The id of the company
#' @import rvest
#' @import jsonlite
#' @import xml2
get_summary_of_ticker <- function(ticker){
  #  ticker<- 'TSLA'
  t <- read_html(paste0('https://www.tradingview.com/symbols/', ticker, '/'))
  return(fromJSON(
    t %>%
      html_nodes(xpath = "//script[@type='application/ld+json']") %>% '[['(3) %>%
      html_text()
  ))
}

#' Get info plot
#' @export
#' @param Ticker The id of the company
#' @param my_json_string json string from inspect view at the bottom
#' @param show_date start date of the plot
#' @import rvest
#' @import jsonlite
#' @import data.table
#' @import rtsdata
#' @import TTR
#' @import pracma
#' @import plotly
get_info_plot  <- function(my_ticker, show_date) {
  my_info <- get_summary_of_ticker(my_ticker)
  show_date <- as.Date(show_date)-365
  t <- get_one_ticker(ticker = my_ticker, start_date = show_date, end_date = Sys.Date())
  t <- t[date>(as.Date(show_date)+365) ,]
  t$fall_from_max <- ((t$low / max(t$high)) -1 )*100
  t$fall_from_close <- ((t$close / max(t$high)) -1 )*100
  t$fall_from_high <- ((t$high / max(t$high)) -1 )*100

  fig <-plot_ly(t, x=~date, type="candlestick",
                open = ~open, close = ~close,
                high = ~high, low = ~low) %>%
    layout(title = "Basic Candlestick Chart", showlegend = FALSE,  xaxis = list(rangeslider = list(visible = F))) %>%
    add_lines(x=~date,y=~ma_200_value, name='200 daily MA') %>%
    add_lines(x=~date,y=~ma_50_value, name='50 daily MA')

  #fig

  fall_plot <-
    plot_ly(t, x=~date,y=~fall_from_max,  type = 'scatter', mode = 'lines', name = 'min')  %>%
    add_lines(x=~date,y=~fall_from_close, name='close') %>%
    add_lines(x=~date,y=~fall_from_high, name='high')  %>%
    layout(title = "", showlegend = F,  xaxis =list(title=""), yaxis =list(title="Fall from max (%)") )


  rsi_plot <-
    plot_ly(t, x=~date,y=~rsi,  type = 'scatter', mode = 'lines', name = 'min') %>%
    add_trace(x=~date,y=30, line = list(color = 'red', width = 4, dash = 'dash') ) %>%
    add_trace(x=~date,y=70, line = list(color = 'red', width = 4, dash = 'dash')) %>%
    layout(title = "", showlegend = F,  xaxis =list(title=""), yaxis =list(title="RSI") )

  all_p<- subplot(list(fig, rsi_plot, fall_plot ), nrows = 3, shareX = T, shareY = F, margin = 0.01, heights = c(0.5, 0.25, 0.25))
  return(list(my_plot = all_p, 'name'= my_info$name, 'desc' = my_info$description, 'ticker' = my_info$tickerSymbol))

}


#' Post a picture of a ticker to slack
#' @export
#' @import ggplot2
#' @import tidyquant
#' @import grid
#' @import gridExtra
#' @import httr
#' @param ticker Ticker id of the company
#' @param slack_token Slack token
#' @param slack_chanel_id Slack chanel id(not the name of it)
get_ggplot_of_ticker_to_slack <- function(ticker, slack_token, slack_chanel_id) {

  my_info <- get_summary_of_ticker(ticker)
  t <- get_one_ticker(ticker = ticker, start_date = (Sys.Date()-700), end_date = Sys.Date())
  t <- t[date>(as.Date((Sys.Date()-700))+365) ,]
  t$fall_from_max <- ((t$low / max(t$high)) -1 )*100
  t$fall_from_close <- ((t$close / max(t$high)) -1 )*100
  t$fall_from_high <- ((t$high / max(t$high)) -1 )*100

  p1 <-   ggplot(t, aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close),colour_up = 'green') +theme_minimal()+
    geom_line(aes(y = ma_200_value), color = "red") +
    geom_line(aes(y = ma_50_value), color = "orange") +
    labs(title=ticker, x= '', y="")

  p2 <- ggplot(t, aes(x=date, y=rsi)) + geom_line()+
    geom_line(aes(y = 70), color = "black", linetype = "dashed") +
    geom_line(aes(y = 30), color = "black", linetype = "dashed") +theme_minimal()+
    labs( x= '', y="")

  grid.newpage()
  grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2)))
  p<- grid.arrange(p1, p2, ncol = 1, heights = c(2, 1.2))

  ggsave(filename = paste0(getwd(), '/', ticker,'.png'), plot = p,  width = 30, height = 35, units = "cm")

  POST(url="https://slack.com/api/files.upload",
       add_headers(`Content-Type`="multipart/form-data"),
       body=list(file=upload_file(paste0(getwd(), '/', ticker,'.png')),
                 token=slack_token, channels=slack_chanel_id))
  file.remove(paste0(getwd(), '/', ticker,'.png'))
}

