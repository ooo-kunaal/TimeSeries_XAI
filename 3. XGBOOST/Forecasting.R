# Load libraries
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

row = c('Selected Variables','RMSE_train','MAPE_train','RMSE_test','MAPE_test',
        'Actual_28/6','Forecast_28/6','f_RMSE','f_MAPE','eta','lambda','alpha','lambda_bias')
final_results = matrix(NA, nrow = 13, ncol=5,dimnames = list(row, tickers))

# Random Forest regression function to check the accuracy of the RFE selected features
run_RF <- function(xr_train, xr_test, yr_train, yr_test) {
  clf <- randomForest(x = xr_train, y = yr_train, ntree = 1000, importance = TRUE)
  yr_pred <- predict(clf, xr_test)
  print(paste('Random Forest Loss:', sqrt(mean((yr_test - yr_pred)^2))))
}
# Fitness function for Genetic Algorithm
fitness_function <- function(params) {
  eta <- round(params[1], 2)    # Round to 2 decimal places
  lambda <- params[2]           # L2 regularization term
  alpha <- params[3]            # L1 regularization term
  lambda_bias <- params[4]      # L2 regularization term on bias
  # Define the parameters
  param <- list(booster = "gblinear", objective = "reg:squarederror", eta = eta, lambda = lambda, alpha = alpha, lambda_bias = lambda_bias )
  # Perform cross-validation
  cv <- xgb.cv(params = param, data = dtrain, nrounds = 200, nfold = 5, metrics = "rmse", verbose = 0, early_stopping_rounds = 10)
  # Return the negative RMSE (since GA maximizes the fitness function)
  return(-min(cv$evaluation_log$test_rmse_mean))
}

# Define parameter bounds for GA Hyperparameter search
lower <- c(0.01, 0, 0, 0)
upper <- c(0.3, 1, 1, 1)

for (ticker in tickers) {
  set.seed(2023)
  # ticker = tickers[1]
  cat("Processing ticker:", ticker, "\n")
  start_time <- Sys.time() # Start time
  # read stock_data csv
  stock_data = read.csv(paste0('StockData ', ticker,'.csv'))
  stock_data$Date = as.Date(stock_data$Date, format = "%d/%m/%Y") # ensure date format
  # separate out 28/6/2024 for future forecasting
  future = stock_data[nrow(stock_data),]
  stock_data = stock_data[-nrow(stock_data),]
  final_results[6,ticker] = future$Close
  # Prepare data for Recursive Feature Elimination
  X_rfe <- stock_data %>% select(-Close, -Date)
  y_rfe <- stock_data$Close
  # Split data into training and testing sets
  split_point <- nrow(stock_data) - 60  # Last 60 days for testing
  xr_train <- X_rfe[1:split_point, ]
  xr_test <- X_rfe[(split_point + 1):nrow(stock_data), ]
  yr_train <- y_rfe[1:split_point]
  yr_test <- y_rfe[(split_point + 1):nrow(stock_data)]
  # Feature scaling
  scaler <- preProcess(xr_train, method = c("center", "scale"))
  xr_train_scaled <- predict(scaler, xr_train)
  xr_test_scaled <- predict(scaler, xr_test)
  scaling_params <- list(center = scaler$mean, scale = scaler$std) # Store the scaling parameters
  
  # Defining the Random Forest RFE Algorithm
  cat("Feature Selection begins!", "\n")
  control <- rfeControl(functions = rfFuncs, method = "cv", number = 5, verbose = TRUE)
  rfe_result <- rfe(x = xr_train_scaled, y = yr_train, sizes = c(10,15,20,25), rfeControl = control)
  selected_features <- predictors(rfe_result) 
  # selected_features <- c("COD","RSI_Close","DEMA_Close_F","OBV","Open","DEMA_Close_M","EMA_Close_F","lag1","SAR","VWAP")
  
  final_results[1, ticker] = paste(selected_features,collapse = ', ')
  cat("RFE completed and selected features for", ticker, "are:", final_results[1, ticker], "\n")
  
  # Transform the training and testing data to keep only the selected features
  xr1_train <- xr_train_scaled[, selected_features]
  xr1_test <- xr_test_scaled[, selected_features]
  
  run_RF(xr1_train, xr1_test, yr_train, yr_test) # check - Random Forest with the selected features
  
  # Create a new dataset with the selected features
  newdata <- stock_data %>% select(Date, all_of(selected_features), Close)
  # Split new data into training and testing sets
  trainData <- newdata[1:split_point, ]
  testData  <- newdata[(split_point + 1):nrow(newdata), ]
  
  # Prepare data for XGBoost
  dtrain <- xgb.DMatrix(data = as.matrix(trainData[, -c(1, ncol(trainData))]), label = trainData$Close)
  dtest <- xgb.DMatrix(data = as.matrix(testData[, -c(1, ncol(testData))]), label = testData$Close)

  # Run the Genetic Algorithm
  cat("Genetic Algorithm Hyperparameter Tuning begins!", "\n")
  a = Sys.time()
  ga_result <- ga(type = "real-valued", fitness = fitness_function, lower = lower, upper = upper, popSize = 20, maxiter = 30, run = 10) # Early stopping for GA
  b = Sys.time()
  cat("Time taken by Genetic Algorithm ", ticker, ":", (b-a)/60, "minutes\n")
  best_params <- ga_result@solution # Extract the best parameters
  eta <- round(best_params[1], 2)   # eta = 0.26
  lambda <- best_params[2]          # lambda = 0.49
  alpha <- best_params[3]           # alpha = 0.267
  lambda_bias <- best_params[4]     # lambda_bias = 0.419
  cat("eta:", eta,"lambda:",lambda,"alpha:",alpha,"lambda_bias:",lambda_bias, "\n")
  final_results[10, ticker] = eta
  final_results[11, ticker] = lambda
  final_results[12, ticker] = alpha
  final_results[13, ticker] = lambda_bias
  # Train final model using the best parameters
  set.seed(2024)
  final_model <- xgb.train(params = list(booster = "gblinear", objective = "reg:squarederror", eta = eta, lambda = lambda, alpha = alpha, lambda_bias = lambda_bias), data = dtrain, nrounds = 100)
  
  # train data performance
  train_preds = predict(final_model, dtrain)
  train_rmse = rmse(yr_train, train_preds)
  train_mape = mape(yr_train, train_preds)
  cat("Train performance for", ticker, "- RMSE:", train_rmse, "MAPE:", train_mape, "\n")
  final_results[2,ticker] = train_rmse
  final_results[3,ticker] = train_mape
  # test data performance
  preds <- predict(final_model, dtest)
  
  test_rmse = rmse(yr_test, preds)
  test_mape = mape(yr_test, preds)
  cat("Test performance for", ticker, "- RMSE:", test_rmse, "MAPE:", test_mape, "\n")
  final_results[4,ticker] = test_rmse
  final_results[5,ticker] = test_mape
  
  # Plot
  plot(testData$Date, testData$Close, type = 'l', col = 'black', lwd = 2, ylim = c(min(preds, testData$Close), max(preds,testData$Close)), sub = 'Model: XGBoost', main = paste('Test Performance for', ticker), ylab = 'Closing Price', xlab = '', xaxt = 'n')
  points(testData$Date, testData$Close, cex = 0.5)
  lines(testData$Date, preds, col = 'red', lwd = 2)
  points(testData$Date, preds, col = 'red', cex = 0.5)
  axis(1, at = testData$Date, labels = format(stock_data$Date[(split_point + 1):length(stock_data$Date)], "%b-%d"), las = 2, cex.axis = 0.7)
  legend("topleft", legend = c("Actual", "Predicted"), col = c("black", "red"), lty = c(1, 1, 1, 1), bty = 'n', lwd = 2)
  
  # dev.off()
  # future forecast
  future = future %>% select(Date, all_of(selected_features), Close)
  dfuture <- xgb.DMatrix(data = as.matrix(future[, -c(1, ncol(future))]), label = future$Close)
  set.seed(2023)
  pfuture = predict(final_model, dfuture)
  f_rmse = rmse(future$Close, pfuture)
  f_mape = mape(future$Close, pfuture)
  cat("Future forecast for", ticker, "- Actual 28/6:", future$Close, "Forecast 28/6:", pfuture, "\n")
  cat("Future forecast performance for", ticker, "- RMSE:", f_rmse, "MAPE:", f_mape, "\n")
  final_results[7,ticker] = pfuture
  final_results[8,ticker] = f_rmse
  final_results[9,ticker] = f_mape
  
  end_time <- Sys.time() # End time
  duration <- (end_time - start_time)/60
  cat("Time taken for", ticker, ":", duration, "minutes\n")
  
  # save.image(paste0("C:/Users/KUNAL/Downloads/Spring Sem/Dissertation/Workspace ",ticker,".RData"))
  
}
