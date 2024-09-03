# Load necessary libraries
library(forecast)
library(dplyr)
library(tseries)
library(Metrics)

tickers = c("RELIANCE.NS", "HINDUNILVR.NS", "ITC.NS", "IFBIND.NS", "HDFCBANK.NS")

for (ticker in tickers) {
  # ticker = tickers[1]
  cat('ticker:', ticker, "\n")
  
  # Read stock data
  stock_data <- read.csv(paste0('StockData ', ticker, '.csv'))
  stock_data$Date <- as.Date(stock_data$Date, format = "%d/%m/%Y")
  
  # Separate out the last row for future forecasting
  future <- stock_data[nrow(stock_data), ]
  stock_data <- stock_data[-nrow(stock_data), ]
  
  close_series <- stock_data$Close
  # Split the data into training and testing sets
  split_point <- length(close_series) - 60
  trainData <- close_series[1:split_point]
  testData <- close_series[(split_point + 1):length(close_series)]
  
  # Manually loop over p and q values with d fixed at 1
  best_aic <- Inf
  best_model <- NULL
  best_order <- c(0, 0, 0)
  d <- 1  # Fixed value for d
  
  for (p in 1:5) {
    for (q in 1:5) {
      try({
        # Fit the ARIMA model
        model <- arima(trainData, order = c(p, d, q))
        aic <- AIC(model)
        
        # Keep track of the best model
        if (aic < best_aic) {
          best_aic <- aic
          best_model <- model
          best_order <- c(p, d, q)
        }
      }, silent = TRUE)
    }
  }
  
  cat("Best ARIMA order for", ticker, ":", best_order, "with AIC:", best_aic, "\n")
  
  
  # Forecast
  forecasts <- forecast(best_model, h = length(testData)+1)
  
  # Calculate RMSE and MAPE for train and test
  train_rmse <- sqrt(mean(best_model$residuals^2))
  train_mape <- mean(abs(best_model$residuals / trainData)) * 100
  
  test_rmse <- rmse(testData, forecasts$mean[-61])
  test_mape <- mape(testData, forecasts$mean[-61]) * 100
  
  write.csv(data.frame(actual=testData, preds = forecasts$mean[-61]),paste('Predictions for',ticker,'.csv'))
  # Forecast unknown future
  future_forecast <- tail(forecasts$mean, 1)
  
  # Calculate RMSE and MAPE for the future forecast
  future_rmse <- rmse(future$Close, future_forecast)
  future_mape <- mape(future$Close, future_forecast)*100
  
  # Print the performance metrics
  cat("Train performance for", ticker, "- RMSE:", train_rmse, "MAPE:", train_mape, "\n")
  cat("Test performance for", ticker, "- RMSE:", test_rmse, "MAPE:", test_mape, "\n")
  cat('28/6 Actual:',future$Close,'28/6 Forecast:',future_forecast)
  cat("Future forecast performance for", ticker, "- RMSE:", future_rmse, "MAPE:", future_mape, "\n")
  
  # Plot test performance
  # png(paste('test performance for', ticker, '.png'), width = 600, height = 600)
  plot(stock_data$Date[(split_point + 1):length(stock_data$Date)], testData, type = 'l', col = 'black', lwd = 2, ylim = c(min(forecasts$lower[-61]), max(forecasts$upper[-61])), sub = paste0('Model: ARIMA(', best_order[1], ',', best_order[2], ',', best_order[3], ')'), main = paste('Test Performance for', ticker), ylab = 'Closing Price', xlab = '', xaxt = 'n')
  points(stock_data$Date[(split_point + 1):length(stock_data$Date)], testData, cex = 0.5)
  polygon(c(stock_data$Date[(split_point + 1):length(stock_data$Date)], rev(stock_data$Date[(split_point + 1):length(stock_data$Date)])),  c(forecasts$lower[,2][-61], rev(forecasts$upper[,2][-61])),  col = rgb(173, 216, 230, max = 255, alpha = 100), border = NA)
  polygon(c(stock_data$Date[(split_point + 1):length(stock_data$Date)], rev(stock_data$Date[(split_point + 1):length(stock_data$Date)])),  c(forecasts$lower[,1][-61], rev(forecasts$upper[,1][-61])),  col = rgb(0, 0, 139, max = 255, alpha = 50), border = NA)
  lines(stock_data$Date[(split_point + 1):length(stock_data$Date)], forecasts$mean[-61], col = 'red', lwd = 2)
  points(stock_data$Date[(split_point + 1):length(stock_data$Date)], forecasts$mean[-61], col = 'red', cex = 0.5)
  axis(1, at = stock_data$Date[(split_point + 1):length(stock_data$Date)],  labels = format(stock_data$Date[(split_point + 1):length(stock_data$Date)], "%b-%d"),  las = 2, cex.axis = 0.7)
  legend("topleft", legend = c("Actual", "Predicted", "80% CI", "90% CI"), col = c("black", "red", "lightblue", "navy"), lty = c(1, 1, 1, 1), bty = 'n', lwd = 2)
  # dev.off()
  
}
