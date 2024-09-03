# Load required libraries
library(fastshap)
library(iml)
library(ggplot2)
library(reshape2)
library(data.table)
library(ggforce)

# Corrected function to apply perturbations based on SHAP values
apply_shap_perturbation <- function(X, shap_values, threshold = 0.7, method = 'swap') {
  X_perturbed <- as.matrix(X)
  for (i in 1:nrow(X)) {
    # Identify important features based on SHAP values
    importance <- abs(shap_values[i, ])
    important_indices <- which(importance > quantile(importance, probs = threshold))
    
    for (idx in important_indices) {
      if (method == 'swap') {
        # Swap with neighboring values
        if (idx > 1 && idx < ncol(X)) {
          X_perturbed[i, c(idx-1, idx, idx+1)] <- X_perturbed[i, c(idx+1, idx, idx-1)]
        }
      } else if (method == 'mean') {
        # Replace with mean value
        X_perturbed[i, idx] <- mean(X_perturbed[, idx], na.rm = TRUE)
      } else if (method == 'permute') {
        # Permute (randomize) the values in the important indices
        X_perturbed[i, important_indices] <- sample(X_perturbed[i, important_indices])
      }
    }
  }
  return(as.data.frame(X_perturbed))
}

# Function to unscale predictions and calculate RMSE and MAPE
calculate_metrics_unscaled <- function(model, X, y_true, mu, sd) {
  # Predict the scaled values
  y_pred_scaled <- predict(model, X)
  
  # Unscale the predictions and the true values
  y_pred <- y_pred_scaled * sd['Close'] + mu['Close']
  y_true_unscaled <- y_true * sd['Close'] + mu['Close']
  
  # Calculate RMSE and MAPE
  rmse <- sqrt(mean((y_true_unscaled - y_pred)^2))
  mape <- mean(abs((y_true_unscaled - y_pred) / y_true_unscaled))
  
  return(list(rmse = rmse, mape = mape))
}

# Step 1: Read SHAP values from CSV file
shap_values_test <- read.csv("shap_values_test.csv")

# Step 2: Ensure the SHAP values and test data are in the correct format
shap_values_test <- as.matrix(shap_values_test)
X_test <- as.matrix(testData[, -11])

# Apply SHAP-based perturbations to the test data (excluding the "Zero" method)
X_test_shap_swap <- apply_shap_perturbation(X_test, shap_values_test, threshold = 0.7, method = 'swap')
X_test_shap_mean <- apply_shap_perturbation(X_test, shap_values_test, threshold = 0.7, method = 'mean')
X_test_shap_permute <- apply_shap_perturbation(X_test, shap_values_test, threshold = 0.7, method = 'permute')

# Step 3: Calculate metrics for the original data after unscaling
metrics_original <- calculate_metrics_unscaled(final_model, X_test, testData[, 11], mu, sd)

# Step 4: Calculate metrics after SHAP perturbations after unscaling
metrics_shap_swap <- calculate_metrics_unscaled(final_model, X_test_shap_swap, testData[, 11], mu, sd)
metrics_shap_mean <- calculate_metrics_unscaled(final_model, X_test_shap_mean, testData[, 11], mu, sd)
metrics_shap_permute <- calculate_metrics_unscaled(final_model, X_test_shap_permute, testData[, 11], mu, sd)

# Print the results
print(paste("Original RMSE:", metrics_original$rmse, "Original MAPE:", metrics_original$mape))
print(paste("RMSE after SHAP Swap:", metrics_shap_swap$rmse, "MAPE after SHAP Swap:", metrics_shap_swap$mape))
print(paste("RMSE after SHAP Mean:", metrics_shap_mean$rmse, "MAPE after SHAP Mean:", metrics_shap_mean$mape))
print(paste("RMSE after SHAP Permute:", metrics_shap_permute$rmse, "MAPE after SHAP Permute:", metrics_shap_permute$mape))

# Step 5: Create comparison plot
comparison_data <- data.frame(
  Method = c('Original', 'Swap', 'Mean', 'Permute'),
  RMSE = c(metrics_original$rmse, metrics_shap_swap$rmse, metrics_shap_mean$rmse, metrics_shap_permute$rmse),
  MAPE = c(metrics_original$mape, metrics_shap_swap$mape, metrics_shap_mean$mape, metrics_shap_permute$mape)
)
comparison_data
