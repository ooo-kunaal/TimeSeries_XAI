# Load necessary libraries
library(forecast)
library(dplyr)
library(Metrics)

tickers = c("RELIANCE.NS", "HINDUNILVR.NS", "ITC.NS", "IFBIND.NS", "HDFCBANK.NS")

for (ticker in tickers) {
  # ticker = tickers[1]
  # Read stock data
  stock_data = read.csv(paste0('StockData ', ticker, '.csv'))
  stock_data$Date = as.Date(stock_data$Date, format = "%d/%m/%Y")
  future = stock_data[nrow(stock_data), ]
  stock_data = stock_data[-nrow(stock_data), ]
  
  # ggtsdisplay(stock_data$Close, smooth = T, main='Reliance Closing Price')
  

  # p1 = ggplot(stock_data, aes(x = Date, y = Close)) +
  #   geom_line(color = "black") +
  #   geom_smooth(method = "gam", se = FALSE, color = "blue") +
  #   labs(title = "Reliance Closing Price", x = "", y = "Closing Price") +
  #   scale_x_date(date_breaks = "year", date_labels = "%b %Y", date_minor_breaks='year') +
  #   theme_minimal() +
  #   theme(plot.title = element_text(size = 14, hjust = 0.5),
  #         axis.text.x = element_text(angle = 45, hjust = 1),
  #         plot.margin = margin(10, 10, 10, 10))
  # p2 = ggAcf(stock_data$Close) + ggtitle("") + ylab("Sample ACF") + xlab("Lags")
  # p3 = ggPacf(stock_data$Close) + ggtitle("") + ylab("Sample PACF") + xlab("Lags")
  # 
  # p4 = ggplot(stock_data, aes(x = Close, y = after_stat(density))) +
  #   geom_histogram(binwidth = 100, fill = "lightblue", color = "navy", alpha = 0.75) +
  #   geom_density(color = "navy", size = 1) +
  #   labs(title = "", x = "Closing Price", y = "Density") +
  #   theme_minimal() +
  #   theme(plot.title = element_text(size = 14, hjust = 0.5),
  #         plot.margin = margin(10, 10, 10, 10))
  # 
  # library("ggpubr")
  # ggarrange(p1, ggarrange(p2, p3, p4, ncol = 3), nrow = 2)
  
  # closing prices of Reliance have seen a non-linear growth highlighted by the GAM-based trend line
  # the ACF plot shows significant autocorrelation at multiple lags suggesting highly correlation with recent past prices 
  # indicating potential momentum in price movements & non-stationarity of the series
  # the PACF plots hint towards an AR(1) model - helpful insight for building the ARIMA model
  # The sharp drop-off after the first lag suggests that most of the predictive information in past prices is contained in the immediately preceding price, which can simplify the modeling process.
  # the Density histogram shows a multi-modal distribution (multiple peaks) indicating historical prices are clustered around specific levels
  
  
  # library(makeR)
  # library(plotly)
  # library(quantmod)
  # 
  # getSymbols(ticker, src = "yahoo", from = "2023-07-01", to = "2024-07-01")
  # stock_data.og <- get(ticker)
  # stock_data.og <- data.frame(Date = index(stock_data.og), coredata(stock_data.og))
  # stock_data.og <- stock_data.og[, -7] # drop last column
  # colnames(stock_data.og) <- c("Date", "Open", "High", "Low", "Close", "Volume")
  # 
  # candle stick chart - last year
  # stock_data.og %>% plot_ly(x=~stock_data.og$Date, type="candlestick", open = ~stock_data.og$Open, close = ~stock_data.og$Close,  high = ~stock_data.og$High, low = ~stock_data.og$Low) %>%
  #   layout(title = 'Reliance 2023-24', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Dates'), yaxis = list(title = 'Closing Price'))
  # 
  # green = closing > opening (bullish days) # red = bearish days
  ## price movement analysis - 
  # July 2023 to November 2023: The chart shows a downward trend with several bearish candlesticks, indicating a period of selling pressure. This suggests that the stock experienced a correction or a decline in investor confidence during this time.
  # November 2023 to March 2024: A strong recovery is visible, with a series of green candlesticks. The price rebounded, moving upwards sharply, possibly due to favorable market conditions or company-specific news.
  # March 2024 to May 2024: The stock entered a consolidation phase, where the price fluctuated within a range. This might suggest that the market was indecisive, with buyers and sellers balancing each other out.
  # May 2024 to July 2024: A renewed upward trend is apparent, with the price making higher highs. The chart shows strong bullish momentum towards the end of the period.
  
  ## Volatility and Market Sentiment:
  # The alternating red and green candlesticks during consolidation phases reflect the tug-of-war between bulls and bears, with neither side gaining a decisive upper hand until the trend resumes.
  
  # volume chart - last year
  # stock_data.og %>% ggplot(aes(x=Date,y=Volume/1000))+ geom_segment(aes(xend = Date, yend = 0, color=Volume))+ geom_smooth(method = 'loess',se=F)+ labs(title = "Reliance Volume Chart", subtitle = "Charting Daily Volume", y = "Volume ('000s)", x = "")+ theme(legend.position = "none")
  # The volume chart effectively visualizes the daily trading volume of Reliance over the period from July 2023 to July 2024. Each vertical segment represents the volume of shares traded on a particular day, providing insights into market activity.
  # spike in volume on 20th Sept 2023 - price dropped by 3% - govt raises windfall tax on sale of domestic crude oil
   
  # stock_data.og[which.max(stock_data.og$Volume),]
  # chartSeries(stock_data.og)
  # addMACD()
  # addBBands()
  # 
  
  # Split the data into training and testing sets
  split_point = nrow(stock_data) - 60
  trainData <- stock_data[1:split_point, ]
  testData  <- stock_data[(split_point + 1):nrow(stock_data), ]
  
  # SNaive Model
  snaive_model = snaive(ts(trainData$Close, frequency = 365), h = length(testData$Close)+1, lambda = 'auto')
  
  # train data evaluation
  train_rmse = forecast::accuracy(snaive_model)[1, "RMSE"]
  train_mape = forecast::accuracy(snaive_model)[1, "MAPE"]
  cat("Train performance for", ticker, "- RMSE:", train_rmse, "MAPE:", train_mape, "\n")

  # test data performance
  testData$Predicted_Close = snaive_model$mean[-61]
  test_rmse = rmse(testData$Close, testData$Predicted_Close)
  test_mape = 100 * mape(testData$Close, testData$Predicted_Close)
  cat("Test performance for", ticker, "- RMSE:", test_rmse, "MAPE:", test_mape, "\n")
  
  write.csv(data.frame(actual=testData$Close, preds = testData$Predicted_Close),paste('Predictions for',ticker,'.csv'))
  
  # Forecast the next value (for "future")
  future_forecast = snaive_model$mean[61]
  future_rmse = rmse(future$Close, future_forecast)
  future_mape = 100 * mape(future$Close, future_forecast)
  cat("28/6 Actual", future$Close, "28/6 Forecast", future_forecast, "\n")
  cat("Future performance for", ticker, "- RMSE:", future_rmse, "MAPE:", future_mape, "\n")
  
  # Plot the test data
  plot(testData$Date, testData$Close, type = 'l', col = 'black', lwd = 2,  ylim = c(min(snaive_model$lower[-61]), max(snaive_model$upper[-61])),  sub = 'Model: SNAIVE',  main = paste('Test Performance for', ticker), ylab = 'Closing Price', xlab = '', xaxt = 'n')
  points(testData$Date, testData$Close, cex = 0.5)
  polygon(c(testData$Date, rev(testData$Date)),  c(snaive_model$lower[,2][-61], rev(snaive_model$upper[,2][-61])),  col = rgb(173, 216, 230, max = 255, alpha = 100), border = NA)
  polygon(c(testData$Date, rev(testData$Date)),  c(snaive_model$lower[,1][-61], rev(snaive_model$upper[,1][-61])),  col = rgb(0, 0, 139, max = 255, alpha = 50), border = NA)
  lines(testData$Date, snaive_model$mean[-61], col = 'red', lwd = 2)
  points(testData$Date, snaive_model$mean[-61], col = 'red', cex = 0.5)
  axis(1, at = testData$Date,  labels = format(testData$Date, "%b-%d"),  las = 2, cex.axis = 0.7)
  legend("topleft", legend = c("Actual", "Predicted", "80% CI", "90% CI"), col = c("black", "red", "lightblue", "navy"), lty = c(1, 1, 1, 1), bty = 'n', lwd = 2)

}
