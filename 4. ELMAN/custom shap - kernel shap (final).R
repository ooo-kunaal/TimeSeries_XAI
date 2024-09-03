
my.shap.plot = function(final_model, data, title='Shap Plot', verbose = T, save.plot=T, save.csv=TRUE, csv.file="shap_values.csv"){
  library(fastshap)
  library(ggplot2)
  library(reshape2)
  library(data.table)
  library(ggforce)
  library(iml)
  # Define the prediction function
  predict_function <- function(model, newdata) { preds <- predict(model, newdata)* sd['Close'] + mu['Close'] }  # unscale predictions

  X_train <- data[, -11]
  y_train <- data[, 11]
  
  model <- Predictor$new(final_model, data = X_train, y = y_train, predict.function = predict_function)
  
  shapley <- Shapley$new(model, x.interest = X_train[1, ], sample.size = 100)
  shap.val <- shapley$results[, 2]
  
  # Calculate SHAP values for all instances
  shap.val <- matrix(NA, nrow = nrow(X_train), ncol = ncol(X_train))
  for (i in 1:nrow(X_train)) {
    if (verbose) cat("Calculating Shap Values for", i, "th data point\n")
    shapley <- Shapley$new(model, x.interest = X_train[i, ], sample.size = 100)
    shap.val[i, ] <- shapley$results[, 2]
  }
  colnames(shap.val) <- colnames(X_train)
  
  # Save SHAP values to a CSV file
  if (save.csv) {
    write.csv(shap.val, csv.file, row.names = FALSE)
    cat("SHAP values saved to", csv.file, "\n")
  }
  
  # Calculate mean absolute SHAP values and order features
  mean_abs_shap <- colMeans(abs(shap.val))
  ordered_features <- names(mean_abs_shap)[order(mean_abs_shap, decreasing = TRUE)]
  
  # Prepare SHAP values for plotting
  shap_values <- as.data.frame(shap.val)
  shap_values$instance <- 1:nrow(shap_values)
  
  # Melt the data for ggplot
  shap_long <- melt(shap_values, id.vars = "instance")
  
  # Ensure the variables are ordered by mean absolute SHAP values
  shap_long$variable <- factor(shap_long$variable, levels = ordered_features)
  
  # Use actual feature values from your dataset
  feature_values <- data[,-11]  # Assuming these are your feature values
  feature_values$instance <- 1:nrow(feature_values)
  
  # Melt the feature values data for ggplot
  feature_values_long <- melt(feature_values, id.vars = "instance")
  
  # Convert to data table for efficient processing
  setDT(feature_values_long)
  
  # Standardize feature values
  feature_values_long[, stdfvalue := (value - min(value)) / (max(value) - min(value)), by = variable]
  
  # Merge SHAP values and feature values
  merged_data <- merge(shap_long, feature_values_long, by = c("instance", "variable"), suffixes = c(".shap", ".feature"))
  
  # Convert to data table for efficient processing
  setDT(merged_data)
  
  # Compute mean absolute SHAP values for each feature
  mean_shap_values <- merged_data[, .(mean_value = mean(abs(value.shap))), by = variable]
  
  
  # Create the summary plot
  p = ggplot(merged_data, aes(x = value.shap, y = variable, color = stdfvalue)) +
    geom_vline(xintercept = 0) +
    ggforce::geom_sina(maxwidth = 0.7, alpha = 0.7) +
    geom_text(data = mean_shap_values, aes(x = -Inf, y = variable, label = sprintf("%.3f", mean_value)),  size = 3, alpha = 0.7, hjust = -0.2, fontface = "bold", check_overlap = TRUE, inherit.aes = FALSE) +
    scale_color_gradient(low = "#FFCC33", high = "#6600CC", breaks = c(0, 1), labels = c("Low", "High"),  guide = guide_colorbar(barwidth = 6, barheight = 0.3)) +
    theme_bw() +
    scale_y_discrete(limits = rev(levels(merged_data$variable))) +
    labs(x = "SHAP value (impact on model output)", y = "", color = "Feature value", title=title) +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),legend.position = "bottom", legend.title = element_text(size = 10),  legend.text = element_text(size = 8), axis.title.x = element_text(size = 10))
  
  if(save.plot) ggsave(filename = paste(title,'.png'), plot = p)
  return(p)
}

# my.shap.plot(final_model, data = trainData, title = 'SHAP Plot for Reliance Train Data')
# my.shap.plot(final_model, data = testData, title = 'SHAP Plot for Reliance Test Data')
