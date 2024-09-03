
library(pre)
tickers = c("RELIANCE.NS","HINDUNILVR.NS", "ITC.NS", "IFBIND.NS","HDFCBANK.NS")

for(i in 1:5){
  cat('Working for',ticker,'\n')
  ticker = tickers[i]
  load(paste0("C:/Users/KUNAL/Downloads/Spring Sem/Dissertation/2. XGBoost Model/Workspace ",ticker,".RData"))
  
  lstm.df = read.csv(paste0(ticker,'_test_results.csv'))
  
  # Plot
  plot(testData$Date, lstm.df$Actual, type = 'l', col = 'black', lwd = 2, ylim = c(min(lstm.df$Predicted, lstm.df$Actual), max(lstm.df$Predicted,lstm.df$Actual)), sub = 'Model: LSTM', main = paste('Test Performance for', ticker), ylab = 'Closing Price', xlab = '', xaxt = 'n')
  points(testData$Date, lstm.df$Actual, cex = 0.5)
  lines(testData$Date, lstm.df$Predicted, col = 'red', lwd = 2)
  points(testData$Date, lstm.df$Predicted, col = 'red', cex = 0.5)
  axis(1, at = testData$Date, labels = format(stock_data$Date[(split_point + 1):length(stock_data$Date)], "%b-%d"), las = 2, cex.axis = 0.7)
  legend("topleft", legend = c("Actual", "Predicted"), col = c("black", "red"), lty = c(1, 1, 1, 1), bty = 'n', lwd = 3)
  
  my.df = cbind(testData, Predicted = lstm.df$Predicted)
  set.seed(2024)
  
  pre_model <- pre(Predicted ~ ., data = my.df[,-c(1, 12)])
  summary(pre_model)
  png(file = paste('PRE for', ticker,'.png'), width = 1400, height = 900)
  plot(pre_model, type = "rules", linear.terms = T, plot.dim=c(3,5),nterms=15)
  dev.off()
  
  predictions <- predict(pre_model, newdata = testData[,-1])
  # png(file = paste('PRE Fit for', ticker,'.png'), width = 600, height = 500)
  plot(testData$Date, lstm.df$Actual, type = 'l', ylim = c(min(my.df$lstm.df$Predicted, lstm.df$Actual), max(my.df$lstm.df$Predicted,lstm.df$Actual)), xlab = '', ylab = '', main = 'PRE Fit')
  points(testData$Date, lstm.df$Actual, cex = 0.5)
  lines(testData$Date, my.df$lstm.df$Predicted, col = 'red', lwd = 2)
  points(testData$Date, my.df$lstm.df$Predicted, col = 'red', cex = 0.5)
  lines(testData$Date, predictions, col = 'darkgreen', lwd = 2)
  points(testData$Date, predictions, col = 'darkgreen', cex = 0.5)
  legend('topleft',c('Test','LSTM Forecast','PRE Forecast'),bty='n',lwd=2,col=c('black','red','darkgreen'))
  # dev.off()
}