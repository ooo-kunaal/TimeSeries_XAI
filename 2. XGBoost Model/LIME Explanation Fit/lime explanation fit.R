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
i = 5
ticker = tickers[i]
load(paste0("C:/Users/KUNAL/Downloads/Spring Sem/Dissertation/2. XGBoost Model/Workspace ",ticker,".RData"))

# LIME
explainer <- lime::lime(as.data.frame(trainData[, -c(1, ncol(trainData))]), final_model, bin_continuous = TRUE)
# Explanation
explanation_train <- lime::explain(trainData[1:nrow(trainData), -c(1, ncol(trainData))], explainer, n_features = 10, feature_select = 'lasso_path')
explanation_test  <- lime::explain(testData[1:nrow(testData), -c(1, ncol(testData))], explainer, n_features = 10, feature_select = 'lasso_path')
# plot_features(explanation)

# Future forecast explanation
# plot_features(lime::explain(future[, -c(1, ncol(future))], explainer, n_features = 10, feature_select = 'lasso_path'))

# Function to extract feature-wise LIME explanations
extract.lime.exp <- function(case.no, feature, exp) {
  explanation = exp
  case <- explanation[explanation$case == case.no, ]
  r2 <- case[case$feature == feature, 3]
  val <- case[case$feature == feature, 'feature_value']
  return(data.frame(feature = feature, feature_value = val, model_r2 = r2))
}

# Set up plotting area
par(mfrow = c(5, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

# Loop through each feature and create plots
for (feature in selected_features) {
  result <- data.frame(feature = character(), feature_value = numeric(), model_r2 = numeric(), stringsAsFactors = FALSE) # initialize
  
  for (i in as.numeric(rownames(testData))[1]:as.numeric(rownames(testData))[nrow(testData)]) {
    result <- rbind(result, extract.lime.exp(i, feature,explanation_test))
  }
  
  result <- result[order(result$feature_value), ]
  
  # Plot
  plot(result$feature_value, result$model_r2, type = 'b', main = feature,
       xlab = "Feature Value", ylab = "Model R^2", col = "blue", pch = 19)
  abline(lm(result$model_r2 ~ result$feature_value), col = 'navy', lwd = 2)
}

# Title for the entire plot
mtext("LIME Explanation Fit - Test Data", outer = TRUE, cex = 1.5)

# Reset plotting area
par(mfrow = c(5, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

# Plot for training data
for (feature in selected_features) {
  result <- data.frame(feature = character(), feature_value = numeric(), model_r2 = numeric(), stringsAsFactors = FALSE)
  
  for (i in as.numeric(rownames(trainData))[1]:as.numeric(rownames(trainData))[nrow(trainData)]) {
    result <- rbind(result, extract.lime.exp(i, feature, explanation_train))
  }
  
  result <- result[order(result$feature_value), ]
  
  # Plot
  plot(result$feature_value, result$model_r2, type = 'b', main = paste("Train Data:", feature),
       xlab = "Feature Value", ylab = "Model R^2", col = "blue", pch = 19)
  abline(lm(result$model_r2 ~ result$feature_value), col = 'navy', lwd = 2)
}

# Title for the entire plot
mtext("LIME Explanation Fit - Training Data", outer = TRUE, cex = 1.5)


# Prepare data for XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(trainData[, -c(1, ncol(trainData))]), label = trainData$Close)
dtest <- xgb.DMatrix(data = as.matrix(testData[, -c(1, ncol(testData))]), label = testData$Close)

extract.lime.fit <- function(case.no, exp) {
  explanation = exp
  case <- explanation[explanation$case == case.no, ]
  model_prediction <- mean(case$model_prediction)
  lime_prediction <- mean(case$prediction)
  return(c(model_prediction, lime_prediction))
}

fit_results <- data.frame(model_prediction = numeric(), lime_prediction = numeric())
unique_cases <- as.numeric(unique(explanation_train$case))

for (i in unique_cases) {
  fit_results <- rbind(fit_results, extract.lime.fit(i, explanation_train))
}

par(mfrow=c(1,1))
plot(fit_results, main = "LIME Explanation vs Original Model - Training Data", xlab = "Original Model Predictions", ylab = "LIME Model Predictions", col = "darkgreen", pch = 19)
abline(a = 0, b = 1, col = "red", lwd = 2)

fit_results_test <- data.frame(model_prediction = numeric(), lime_prediction = numeric())
unique_cases <- as.numeric(unique(explanation_test$case))

for (i in unique_cases) {
  fit_results_test <- rbind(fit_results_test, extract.lime.fit(i, explanation_test))
}

par(mfrow=c(1,1))
plot(fit_results_test, main = "LIME Explanation vs Original Model - Testing Data", xlab = "Original Model Predictions", ylab = "LIME Model Predictions", col = "darkgreen", pch = 19)
abline(a = 0, b = 1, col = "red", lwd = 2)

combined_plots <- lapply(selected_features, function(feature) {
  ggplot() + geom_density(data = trainData, aes_string(x = feature), fill = "blue", alpha = 0.5) +
    geom_density(data = testData, aes_string(x = feature), fill = "red", alpha = 0.5) +
    ggtitle(paste("Feature:", feature)) +
    theme_minimal()
})
grid.arrange(grobs = combined_plots, ncol = 2, top = "Density Plots for Training (Blue) vs Test (Red) Data")

ks_results <- lapply(selected_features, function(feature) {
  ks_test <- ks.test(trainData[[feature]], testData[[feature]])
  return(data.frame(Feature = feature, D = ks_test$statistic, p_value = round(ks_test$p.value, 5) ))
})
ks_results_df <- do.call(rbind, ks_results)
ks_results_df
