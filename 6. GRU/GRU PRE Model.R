library(pre)

tickers = c("RELIANCE.NS", "HINDUNILVR.NS", "ITC.NS", "IFBIND.NS", "HDFCBANK.NS")

for (ticker in tickers) {
  load(paste0("C:/Users/KUNAL/Downloads/Spring Sem/Dissertation/2. XGBoost Model/Workspace ", ticker, ".RData"))
  print(paste('working for', ticker))
  
  gru.df = read.csv(paste0(ticker, '_gru_test_results.csv'))
  my.df = cbind(testData, Predicted = gru.df$Predicted)
  set.seed(2024)
  
  pre_model <- pre(Predicted ~ ., data = my.df[,-c(1, 12)])
  
  # Plotting the rules from the PRE model
  png(file = paste('PRE_for_', ticker, '.png'), width = 1400, height = 900)
  plot(pre_model, type = "rules", linear.terms = TRUE, plot.dim = c(3, 5), nterms = 15)
  dev.off()
  
  # Plotting the PRE model fit
  predictions <- predict(pre_model, newdata = testData[,-c(1, 12)])
  png(file = paste('PRE_Model_Fit_for_GRU_', ticker, '.png'), width = 600, height = 500)
  plot(testData$Date, gru.df$Actual, type = 'l', ylim = c(min(gru.df$Predicted, gru.df$Actual), max(gru.df$Predicted, gru.df$Actual)), 
       xlab = '', ylab = '', main = paste('PRE Model Fit for GRU:', ticker))
  points(testData$Date, gru.df$Actual, cex = 0.5)
  lines(testData$Date, gru.df$Predicted, col = 'red', lwd = 2)
  points(testData$Date, gru.df$Predicted, col = 'red', cex = 0.5)
  lines(testData$Date, predictions, col = 'darkgreen', lwd = 2)
  points(testData$Date, predictions, col = 'darkgreen', cex = 0.5)
  legend('topleft', c('Test', 'GRU Forecast', 'PRE Forecast'), bty = 'n', lwd = 2, col = c('black', 'red', 'darkgreen'))
  
  # Ensure the plot is rendered before closing the device
  flush.console()
  dev.off()
}

print('complete')
