# Load libraries
library(Metrics)
library(reshape2)
library(quantmod)
library(RSNNS)
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
ticker = tickers[5]
load(paste0("C:/Users/KUNAL/Downloads/Spring Sem/Dissertation/4. Elman Neural Network/elmanWorkspace ",ticker,".RData"))

predict_function <- function(object, newdata) { preds <- predict(object, newdata)* sd['Close'] + mu['Close'] }

X_train <- as.data.frame(trainData[,-11])
y_train <- trainData[,11] * sd['Close'] + mu['Close']  # unscale actual values
X_test <- as.data.frame(testData[,-11])
y_test <- testData[,11] * sd['Close'] + mu['Close']  # unscale actual values

# LIME
model_type.elman <- function(x, ...) { return("regression") }
assign("model_type.elman", model_type.elman, envir = .GlobalEnv)
explainer <- lime(X_train, final_model, bin_continuous = TRUE, quantile_bins = FALSE, predict_function = predict_function)

# custom functions
unscale <- function(value, mean, sd) { return(value * sd + mean) }

extract_numbers <- function(string) { as.numeric(unlist(regmatches(string, gregexpr("[+-]?[0-9]*\\.?[0-9]+", string)))) }

post_process_explanation = function(explanation){
  for (i in seq_len(nrow(explanation))) {
    feature <- explanation$feature[i]
    feature_desc <- explanation$feature_desc[i]
    
    numbers <- extract_numbers(feature_desc)
    unscaled_numbers <- unscale(numbers, mu[feature], sd[feature])
    for (j in seq_along(numbers)) {
      feature_desc <- sub(numbers[j], round(unscaled_numbers[j], 2), feature_desc)
    }
    explanation$feature_desc[i] <- feature_desc
  }
  explanation$prediction = explanation$prediction*sd['Close']+mu['Close']
  return(explanation)
}

# Function to extract feature-wise LIME explanations
extract.lime.exp <- function(case.no, feature, exp) {
  explanation = exp
  case <- explanation[explanation$case == case.no, ]
  r2 <- case[case$feature == feature, 3]
  val <- case[case$feature == feature, 'feature_value']
  return(data.frame(feature = feature, feature_value = val, model_r2 = r2))
}

explanation_test  <- lime::explain(testData[1, -11], explainer, n_features = 10, feature_select = 'lasso_path')

for(i in 2:nrow(testData)){
  print(i)
  explanation_test  <- rbind(explanation_test, lime::explain(testData[i, -11], explainer, n_features = 10, feature_select = 'lasso_path'))
}

model_predictions_test = numeric()
lime_predictions_test = numeric()
for(i in as.numeric(unique(explanation_test$case))){
  model_predictions_test = rbind(model_predictions_test,colMeans(data.frame(explanation_test[explanation_test$case==i,'model_prediction']))*sd['Close']+mu['Close'])
  lime_predictions_test = rbind(lime_predictions_test,colMeans(data.frame(explanation_test[explanation_test$case==i,'prediction']))*sd['Close']+mu['Close'])
}
png(paste('Lime test Scatterplot for',ticker,'.png'))
plot(data.frame(model_predictions_test, lime_predictions_test),main = "LIME Explanation vs Original Model - Test Data", 
     xlab = "Original Model Predictions", ylab = "LIME Model Predictions", 
     col = "darkgreen", pch = 19)
abline(0,1,col='red',lwd=2)
dev.off()

explanation_test = post_process_explanation(explanation_test)

result <- data.frame(feature = character(), feature_value = numeric(), model_r2 = numeric(), stringsAsFactors = FALSE) # initialize

par(mfrow = c(5, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
for(feature in selected_features){
  for(i in as.numeric(unique(explanation_test$case))){
    print(i)
    result <- rbind(result, extract.lime.exp(i, feature, explanation_test))
  }
  result <- result[order(result$feature_value), ]
  # Plot
  plot(result$feature_value, result$model_r2, type = 'b', main = feature,
       xlab = "Feature Value", ylab = "Model R^2", col = "blue", pch = 19)
  abline(lm(result$model_r2 ~ result$feature_value), col = 'navy', lwd = 2)
}

mtext("LIME Explanation Fit - Test Data", outer = TRUE, cex = 1.5)

explanation_train  <- lime::explain(trainData[1, -11], explainer, n_features = 10, feature_select = 'lasso_path')

for(i in 2:nrow(trainData)){
  print(i)
  explanation_train  <- rbind(explanation_train, lime::explain(trainData[i, -11], explainer, n_features = 10, feature_select = 'lasso_path'))
}

model_predictions_train = numeric()
lime_predictions_train = numeric()
for(i in as.numeric(unique(explanation_train$case))){
  model_predictions_train = rbind(model_predictions_train,colMeans(data.frame(explanation_train[explanation_train$case==i,'model_prediction']))*sd['Close']+mu['Close'])
  lime_predictions_train = rbind(lime_predictions_train,colMeans(data.frame(explanation_train[explanation_train$case==i,'prediction']))*sd['Close']+mu['Close'])
}
png(paste('Lime train Scatterplot for',ticker,'.png'))
plot(data.frame(model_predictions_train, lime_predictions_train),main = "LIME Explanation vs Original Model - Train Data", 
     xlab = "Original Model Predictions", ylab = "LIME Model Predictions", 
     col = "darkgreen", pch = 19)
abline(0,1,col='red',lwd=2)
dev.off()

explanation_train = post_process_explanation(explanation_train)

result <- data.frame(feature = character(), feature_value = numeric(), model_r2 = numeric(), stringsAsFactors = FALSE) # initialize

par(mfrow = c(5, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
for(feature in selected_features){
  for(i in as.numeric(unique(explanation_train$case))){
    print(i)
    result <- rbind(result, extract.lime.exp(i, feature, explanation_train))
  }
  result <- result[order(result$feature_value), ]
  # Plot
  plot(result$feature_value, result$model_r2, type = 'b', main = feature,
       xlab = "Feature Value", ylab = "Model R^2", col = "blue", pch = 19)
  abline(lm(result$model_r2 ~ result$feature_value), col = 'navy', lwd = 2)
}

mtext("LIME Explanation Fit - Train Data", outer = TRUE, cex = 1.5)