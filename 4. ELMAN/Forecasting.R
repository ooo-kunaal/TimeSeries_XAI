library(RSNNS)
library(Metrics)
library(dplyr)
library(tidyr)
library(caret)
library(foreach)
library(doParallel)

tickers = c("RELIANCE.NS","HINDUNILVR.NS", "ITC.NS", "IFBIND.NS","HDFCBANK.NS")
selected_features_df = cbind(c("RSI_Close","Open","macd", "rpi", "lag1", "Nifty50", "AvgTrueRange", "DEMA_Close_F", "eps", "COD"),
                             c("COD","RSI_Close","DEMA_Close_F","EMA_Close_F","Open","DEMA_Close_M","SAR","lag1","OBV","macd"),
                             c("COD","RSI_Close","DEMA_Close_F","Open","EMA_Close_F","Nifty50","DEMA_Close_M","macd","up","lag1"),
                             c("COD","RSI_Close","DEMA_Close_F","DEMA_Close_M","EMA_Close_F","Volume","OBV","Open","Nifty50","RVOL"),
                             c("COD","RSI_Close","DEMA_Close_F","OBV","Open","DEMA_Close_M","EMA_Close_F","lag1","SAR","VWAP") )
colnames(selected_features_df) = c("RELIANCE.NS","HINDUNILVR.NS", "ITC.NS", "IFBIND.NS","HDFCBANK.NS")

final_result = matrix(NA, nrow=length(tickers), ncol=11)
dimnames(final_result) = list(tickers, c('ticker','l1','l2','mape train','rmse train','mape test','rmse test','actual','forecast','mape','rmse'))

for (ticker in tickers) {
  # ticker = tickers[5]
  print(paste('Processing', ticker))
  stock_data = read.csv(paste0('StockData ', ticker,'.csv'))
  stock_data$Date = as.Date(stock_data$Date, format = "%d/%m/%Y")
  
  future = stock_data[nrow(stock_data),]
  stock_data = stock_data[-nrow(stock_data),]
  
  selected_features <- selected_features_df[,ticker]
  newdata <- stock_data %>% select(Date, all_of(selected_features), Close)
  future = future %>% select(all_of(selected_features), Close)
  head(newdata, 1)
  
  dates = newdata[,1]
  newdata = scale(newdata[,-1])
  mu = attributes(newdata)$'scaled:center'
  sd = attributes(newdata)$'scaled:scale'
  
  # Split
  split_point <- nrow(stock_data) - 60
  trainData <- data.frame(newdata[1:split_point, ])
  testData  <- data.frame(newdata[(split_point + 1):nrow(newdata), ])
  
  n <- nrow(trainData)
  h <- nrow(testData)
  
  # Grid Search Loop
  # param_grid <- expand.grid(l1 = 2:10, l2 = 0:30)
  # # Setup parallel backend
  # num_cores <- detectCores() - 1
  # cl <- makeCluster(num_cores)
  # registerDoParallel(cl)
  # results <- data.frame()
  # 
  # grid_search_function <- function(l1, l2) {
  #   hidden <- if (l2 == 0) l1 else c(l1, l2)
  #   print(paste('l1: ', l1, 'l2:', l2))
  #   set.seed(2024)
  #   fit.elman <- elman(x = trainData[,-11], y = trainData[,11], size = hidden, maxit = 1000, learnFuncParams = c(0.01))
  #   
  #   set.seed(2024)
  #   preds_train <- predict(fit.elman, trainData[, -11])
  #   preds_test <- predict(fit.elman, testData[,-11])
  #   
  #   preds_train  = preds_train * sd['Close'] + mu['Close']
  #   actual_train = trainData[,11] * sd['Close'] + mu['Close']
  #   preds_test   = preds_test * sd['Close'] + mu['Close']
  #   actual_test  = testData[,11] * sd['Close'] + mu['Close']
  #   
  #   mape_train <- 100 * round(mape(actual = actual_train, predicted = preds_train), 4)
  #   mape_test <- 100 * round(mape(actual = actual_test, predicted = preds_test), 4)
  #   rmse_train <- round(Metrics::rmse(actual = actual_train, predicted = preds_train), 2)
  #   rmse_test <- round(Metrics::rmse(actual = actual_test, predicted = preds_test), 2)
  #   
  #   print(paste('train mape:', mape_train, 'test mape: ', mape_test))
  #   print(paste('train rmse:', rmse_train, 'test rmse: ', rmse_test))
  #   
  #   set.seed(2024)
  #   forecast = predict(fit.elman, future[,-11]) * sd['Close'] + mu['Close']
  #   
  #   return(data.frame(l1 = l1, l2 = l2, MAPE_Train = mape_train, MAPE_Test = mape_test, RMSE_Train = rmse_train, RMSE_Test = rmse_test, forecast = forecast))
  # }
  # print('Grid Search begins!')
  # results <- foreach(param = iter(param_grid, by = 'row'), .combine = rbind, .packages = c('RSNNS', 'forecast',"Metrics")) %dopar% { grid_search_function(param$l1, param$l2) }
  # 
  # stopCluster(cl)
  # 
  # write.csv(results, file=paste('Elman Results',ticker,'.csv'))
  # 
  # # Find the best model
  # results = read.csv(paste('Elman Results',ticker,'.csv'))
  # best_model <- results[which.min(results$RMSE_Test),]
  # print(paste('best model:', best_model$l1, best_model$l2))
  # hidden <- if (best_model$l2 == 0) best_model$l1 else c(best_model$l1, best_model$l2)
  hidden <- c(6, 4)
  set.seed(2024)
  final_model <- elman(x = trainData[,-11], y = trainData[,11], size = hidden, maxit = 1000, learnFuncParams = c(0.01),seed=2024)
  
  # train data performance
  preds_train <- predict(final_model, trainData[, -11]) 
  preds_train = preds_train*sd['Close']+mu['Close']     # unscale predictions 
  actual_train = trainData[,11]*sd['Close']+mu['Close'] # unscale actual values
  mape_train <- 100 * round(mape(actual = actual_train, predicted = preds_train), 4)
  rmse_train <- round(Metrics::rmse(actual = actual_train, predicted = preds_train),2)
  cat("Train performance for", ticker, "- RMSE:", rmse_train, "MAPE:", mape_train, "\n")
  
  # test data performance
  preds_test <- predict(final_model, testData[,-11])
  preds_test  = preds_test *sd['Close']+mu['Close']   # unscale predictions 
  actual_test = testData[,11]*sd['Close']+mu['Close'] # unscale actual values
  mape_test <- 100 * round(mape(actual = actual_test, predicted = preds_test), 4)
  rmse_test <- round(Metrics::rmse(actual = actual_test, predicted = preds_test),2)
  cat("Test performance for", ticker, "- RMSE:", rmse_test, "MAPE:", mape_test, "\n")
  
  # test performance plot
  png(file = paste('Elman',paste(final_model$archParams$size, collapse = ','),  'Performance for', ticker,'.png'), width = 600, height = 500)
  dev.off()
  
  plot(1:60, actual_test, type = 'l', ylim = c(min(actual_test, preds_test), max(actual_test, preds_test)), xlab = '', ylab = '',main=paste('Test Performance:',paste(final_model$archParams$size, collapse = ',')))
  points(1:60, actual_test, cex = 0.5)
  lines(1:60, preds_test, col = 'red', lwd = 2)
  points(1:60, preds_test, col = 'red', cex = 0.5)
  legend('topleft',c('Test','Elman Forecast'),bty='n',lwd=2,col=c('black','red'))
  
  dates = tail(stock_data$Date,60)
  plot(tail(dates,60), actual_test, type = 'l', col = 'black', lwd = 2,  ylim = c(min(actual_test, preds_test), max(actual_test, preds_test)), sub = paste0('Model: Elman RNN'),  main = paste('Test Performance for', ticker), ylab = 'Closing Price', xlab = '', xaxt = 'n')
  points(tail(dates,60), actual_test, cex = 0.5)
  lines(tail(dates,60), preds_test, col = 'red', lwd = 2)
  points(tail(dates,60), preds_test, col = 'red', cex = 0.5)
  axis(1, at = tail(dates,60),  labels = format(tail(dates,60), "%b-%d"),  las = 2, cex.axis = 0.7)
  legend("topleft", legend = c("Actual", "Predicted"), col = c("black", "red"), lty = c(1, 1), bty = 'n', lwd = 3)
  
  # future forecast
  set.seed(2024)
  f = predict(final_model, future[,-11],seed=2024)*sd['Close']+mu['Close']
  f_rmse = Metrics::rmse(future$Close, f)
  f_mape = mape(future$Close, f)
  cat("Future forecast for", ticker, "- Actual 28/6:", future$Close, "Forecast 28/6:", f, "\n")
  cat("Future forecast performance for", ticker, "- RMSE:", f_rmse, "MAPE:", f_mape, "\n")
  
  # final_result[ticker,] = c(ticker, best_model$l1, best_model$l2, mape_train, rmse_train, mape_test, rmse_test, future$Close, f, f_mape,f_rmse)
  save.image(paste0("C:/Users/KUNAL/Downloads/Spring Sem/Dissertation/Elman Neural Network/elmanWorkspace ", ticker,".RData"))
}  