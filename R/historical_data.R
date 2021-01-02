
#' Downloading and adding technical indicators to the historical data of a ticker
#' @export
#' @param ticker The ticker of the company to download
#' @param start_date The first date of the historical data
#' @param end_date The last date of the data
#' @param mas List of the simple moving averages to calculate
#' @param emas List of the exponentional moving averages to calculate
#' @param keepma Logical value if it is F it will delete the actual ma values
#' @param keepema Logical value if it is F it will delete the actual ema values
#' @param addmacd Calculate macd nFast=12, nSlow=26,nSig=9
#' @param addrsi Add relativ sthrength index  n=14
#' @param add_boilinger Adding the diff from the boilinger bands
#' @param add_macd_cross Adding if the macd is crossed
#' @param remove_nas Remove all na values after calculating every sma, ema, rsi, etc
#' @param calc_diff If it false it does not calculate the sma and ema
#' @import data.table
#' @import rtsdata
#' @import TTR
#' @import pracma
get_one_ticker  <- function(ticker, start_date = "1900-01-01", end_date = Sys.Date(),  mas=c(50, 100, 200), emas=c(3, 5, 7, 14), keepma=T, keepema=T, addmacd=T, addrsi=T, remove_nas = T, calc_diff = T, add_boilinger= T, add_macd_cross=T) {
  tryCatch({
    adatom <- data.frame(ds.getSymbol.yahoo(ticker, from = start_date, to =end_date ))
    names(adatom) <- tolower(sapply(strsplit(names(adatom), '.', fixed = T), '[[', 2))
    adatom$date <- as.Date(row.names(adatom))
    row.names(adatom) <- 1:nrow(adatom)
    adatom <- data.table(adatom)

    if( !identical(names(adatom) , c("open","high","low", "close","volume",  "adjusted","date"))) {
      text<- paste0('Error: ', ticker, ' # problem: names of dataframe is bad ', ' time: ', Sys.time())
      stop(text)
    }
    if ( nrow(adatom[complete.cases(adatom)==F,])> 0)  {
      adatom <- adatom[complete.cases(adatom),]
      if(nrow(adatom)==0){
        text<- paste0('Error: ', ticker, ' # problem: empty dataframe ', ' time: ', Sys.time())
        stop(text)
      }
    }
  }, error=function(x) {
    print(x)
    stop('No ticker')
  })

  for (simple_mas in mas) {
    if (nrow(adatom)<=simple_mas) {
      next
    }
    adatom[[paste0('ma_', simple_mas, '_value')]] <- movavg(adatom[['close']], simple_mas, type="s")
    if (calc_diff==T) {
      adatom[[paste0('diff_',simple_mas,'_ma_value')]] <-  (( adatom[["close"]]  /adatom[[paste0('ma_', simple_mas, '_value')]] )-1)*100
      if (keepma==F) {
        adatom[[paste0('ma_', simple_mas, '_value')]] <- NULL
      }
    }
  }

  for (exp_mas in emas) {
    if (nrow(adatom)<=exp_mas) {
      next
    }
    adatom[[paste0('ma_', exp_mas, '_exp_value')]] <- movavg(adatom[['close']], exp_mas, type="e")
    if (calc_diff==T) {
      adatom[[paste0('diff_',exp_mas,'_exp_ma_value')]] <-  (( adatom[["close"]]  / adatom[[paste0('ma_', exp_mas, '_exp_value')]] )-1)*100
      if (keepema==F) {
        adatom[[paste0('ma_', exp_mas, '_exp_value')]]<- NULL
      }
    }
  }

  if (addmacd==T) {
    adatom <- cbind(adatom, data.table(MACD(adatom[['close']], nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)))
  }

  if (add_boilinger) {
    boil <- data.table(BBands( adatom[,c("high","low","close")] ))
    adatom$boil_dow_diff <- (((adatom$close /boil$dn)-1)*100)
    adatom$boil_up_diff <- (((adatom$close /boil$up)-1)*100)
    adatom$boil_mid_diff <- (((adatom$close /boil$mavg)-1)*100)
  }

  if (addrsi==T) {
    adatom$rsi <- RSI(adatom[['close']])
  }

  if (add_macd_cross==T) {
    adatom$macd_cross <- F
    for (i in 2:nrow(adatom)) {
      adatom$macd_cross[i] <- (adatom$macd[(i-1)]< adatom$signal[(i-1)]) & (adatom$macd[i]> adatom$signal[i])
    }
    adatom$macd_cross <- ifelse(adatom$macd_cross, 1, 0)
  }
  adatom <- adatom[complete.cases(adatom)]
  return(adatom)

}

#' Download all the S&P historical data it use the  get_one_ticker function to all the S&P500 tickers
#' @export
#' @param folder_to_save Path of the folder where data should be save
#' @param start_date The first date of the historical data
#' @param end_date The last date of the data
#' @param mas List of the simple moving averages to calculate
#' @param emas List of the exponentional moving averages to calculate
#' @param keepma Logical value if it is F it will delete the actual ma values
#' @param keepema Logical value if it is F it will delete the actual ema values
#' @param addmacd Calculate macd nFast=12, nSlow=26,nSig=9
#' @param addrsi Add relativ sthrength index  n=14
#' @param add_boilinger Adding the diff from the boilinger bands
#' @param add_macd_cross Adding if the macd is crossed
#' @param remove_nas Remove all na values after calculating every sma, ema, rsi, etc
#' @param calc_diff If it false it does not calculate the sma and ema
#' @import data.table
#' @import rtsdata
#' @import TTR
#' @import pracma
download_sp500_hist_data <- function(folder_to_save, start_date = "1900-01-01", end_date = Sys.Date(),  mas=c(50, 100, 200), emas=c(3, 5, 7, 14), keepma=T, keepema=T, addmacd=T, addrsi=T, remove_nas = T, calc_diff = T, add_boilinger= T, add_macd_cross=T) {
  sp_500 <- get_sp500()
  for (i in sp_500) {
    tryCatch({
      print(paste0('Processing: ', i))
      t <- get_one_ticker(ticker = i, start_date = start_date, end_date = end_date, mas=mas, emas=emas, keepma=keepma, keepema= keepema, addmacd = addmacd, addrsi= addrsi, remove_nas = remove_nas, calc_diff= calc_diff, add_boilinger = add_boilinger, add_macd_cross = add_macd_cross)
      saveRDS(t, paste0(folder_to_save, i, '.rds'))
    },error=function(x){
      print(paste0('Error in processing: ', i))
    })
  }
}





