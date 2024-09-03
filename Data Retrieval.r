library(Metrics)
library(reshape2)
library(quantmod)
library(TTR)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(xgboost)
library(SHAPforxgboost)
library(data.table)
library(dplyr)
library(zoo)
library(readr)
library(lime)
library(GA)
library(iml)
library(ggplot2)

tickers = c("RELIANCE.NS","HINDUNILVR.NS", "ITC.NS", "IFBIND.NS","HDFCBANK.NS")
# ticker = tickers[5]

# VWAP function
VWAP <- function(price, volume) {
  cum_price_vol <- cumsum(price * volume)
  cum_vol <- cumsum(volume)
  vwap <- cum_price_vol / cum_vol
  return(vwap)
}
# Relative Volume (RVOL) function
RVOL <- function(volume, n = 20) {
  avg_vol <- rollapply(volume, width = n, FUN = mean, align = 'right', fill = NA)
  rvol <- volume / avg_vol
  return(rvol)
}
# Intra-day Price Range (IPR) function
IPR <- function(high, low) { return(high - low) }

for(ticker in tickers){
  # Stock price data acquisition - 1st Jan 2018 to July 1st 2024
  getSymbols(ticker, src = "yahoo", from = "2018-01-01", to = "2024-07-01")
  stock_data <- get(ticker)
  stock_data <- data.frame(Date = index(stock_data), coredata(stock_data))
  stock_data <- stock_data[, -7] # drop last column
  colnames(stock_data) <- c("Date", "Open", "High", "Low", "Close", "Volume")
  head(stock_data, 2)
  range(stock_data$Date)
  
  # Nifty 50 Index
  getSymbols("^NSEI", src = "yahoo", from = "2018-01-01", to = "2024-07-13")
  nifty50 <- data.frame(Date = index(NSEI), coredata(NSEI[, 4]))
  colnames(nifty50) <- c("Date", "Nifty50")
  stock_data <- stock_data %>% left_join(nifty50, by = "Date")
  # NA value imputation
  stock_data[1, 7] = 10530.70 # last observed value in 2017
  stock_data <- stock_data %>% arrange(Date) %>% mutate(Nifty50 = na.locf(Nifty50, na.rm = FALSE)) # carry forward last observed value
  
  # Technical indicators
  # 1. Volume weighted average price
  stock_data$VWAP <- VWAP(stock_data$Close, stock_data$Volume)
  # 2. Relative Volume
  stock_data$RVOL <- RVOL(stock_data$Volume, n = 20)
  # 3. Intra-day Price Range (IPR)
  stock_data$IPR <- IPR(stock_data$High, stock_data$Low)
  # 4. Simple Moving Averages (SMA)
  stock_data$SMA_Close_F <- SMA(stock_data$Close, n = 5)
  stock_data$SMA_Close_M <- SMA(stock_data$Close, n = 10)
  stock_data$SMA_Close_S <- SMA(stock_data$Close, n = 20)
  # 5. Exponential Moving Averages (EMA)
  stock_data$EMA_Close_F <- EMA(stock_data$Close, n = 5)
  stock_data$EMA_Close_M <- EMA(stock_data$Close, n = 10)
  stock_data$EMA_Close_S <- EMA(stock_data$Close, n = 20)
  # 6. Double Exponential Moving Averages (DEMA)
  stock_data$DEMA_Close_F <- DEMA(stock_data$Close, n = 5)
  stock_data$DEMA_Close_M <- DEMA(stock_data$Close, n = 10)
  stock_data$DEMA_Close_S <- DEMA(stock_data$Close, n = 20)
  # 7. MACD
  macd <- MACD(stock_data$Close, nFast = 12, nSlow = 26, nSig = 9)[, 1] # retain only macd
  stock_data <- cbind(stock_data, macd)
  # 8. Bollinger Bands
  bbands <- BBands(stock_data$Close, n = 20, sd = 2)
  stock_data <- cbind(stock_data, bbands[, c(1, 3)]) # dn, up
  # 9. Relative Strength Index (RSI) - ratio of recent upward price movement to absolute price movement
  stock_data$RSI_Close <- RSI(stock_data$Close, n = 14)
  # 10. True Range and Average True Range (ATR)
  stock_data$TrueRange <- ATR(stock_data[, c("High", "Low", "Close")])[, "tr"]
  stock_data$AvgTrueRange <- ATR(stock_data[, c("High", "Low", "Close")])[, "atr"]
  # 11. On-Balance Volume (OBV)
  stock_data$OBV <- OBV(stock_data$Close, stock_data$Volume)
  # 12. Parabolic SAR
  stock_data$SAR <- as.numeric(SAR(stock_data[, c("High", "Low")]))
  # 13. Close-Open Difference (COD)
  stock_data$COD <- stock_data$Close - stock_data$Open
  # 14. Daily Volatility % (VOL)
  stock_data$VOL <- (stock_data$High - stock_data$Low) / stock_data$Open * 100
  
  # Drop unnecessary columns
  stock_data <- stock_data %>% select(Date, everything(), -High, -Low) %>% na.omit()
  # 15. Lagged closing price
  stock_data <- stock_data %>% mutate(lag1 = lag(Close, n = 1), lag2 = lag(Close, n = 2), lag3 = lag(Close, n = 3), lag4 = lag(Close, n = 4), lag5 = lag(Close, n = 5)) %>% na.omit()
  # range(stock_data$Date)
  # nrow(stock_data)
  
  # 16. Retail price index - indicator of inflation
  rpi = read.csv('RPI.csv')
  stock_data$rpi = rpi$rpi[1:length(stock_data$Date)]
  # 17. earning ratio - Earning per share, Price per share
  earning = read.csv(paste0('earningRatio ',ticker,'.csv')) 
  stock_data$eps = earning$EPS[1:length(stock_data$Date)]
  
  head(stock_data,2)
  # write csv
  write.csv(stock_data, file = paste0('StockData ',ticker,'.csv'))
}