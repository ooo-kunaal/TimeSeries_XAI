tickers = c("RELIANCE.NS","HINDUNILVR.NS", "ITC.NS", "IFBIND.NS","HDFCBANK.NS")
ticker = tickers[5]
shap_values = read.csv(paste0(ticker,"_shap_values_test.csv"))
load(paste0("C:/Users/KUNAL/Downloads/Spring Sem/Dissertation/4. Elman Neural Network/elmanWorkspace ",ticker,".RData"))

p = list()

for(i in 1:10){
  # i=1
  feature_name = colnames(shap_values)[i]
  feature_values = testData[,feature_name]
  
  # Normalize SHAP values for coloring
  norm_shap_values <- (shap_values[,feature_name] - min(shap_values[,feature_name])) / (max(shap_values[,feature_name]) - min(shap_values[,feature_name]))
  
  # Create data frame for ggplot
  plot_data <- data.frame(Time = 1:60, 
                          FeatureValues = feature_values, 
                          NormSHAP = norm_shap_values)
  
  # Create the plot
  p[[i]] <- ggplot(plot_data, aes(x = Time, y = FeatureValues)) +
    geom_tile(aes(fill = NormSHAP), height = Inf) +
    geom_line(color = 'black') +
    geom_point(color = 'black') +
    scale_fill_gradientn(colors = c("blue", "orange", "red"), limits = c(0, 1), name = "Normalized\nSHAP Value") +
    labs(title = feature_name, y = feature_name) +
    theme_minimal()
  
}

# Plot actual closing prices

plot_actual <- ggplot(tail(stock_data,60), aes(x = 1:60, y = Close)) +
  geom_line(color = 'black') + geom_line(aes(x=1:60, y=preds_test), color='red')+
  labs(title = "Closing Price Over Time", y = "Price",x='') +
  theme_minimal()

# Combine all plots
library(gridExtra)
library(cowplot)
# Combine all plots
combined_plot <- plot_grid(plotlist = c(list(plot_actual, p[[1]],p[[2]])), ncol = 1, align = "v")
png(file = paste('SHAP Over Time p1', ticker,'.png'), width = 600, height = 500)
plot_grid(combined_plot, rel_widths = c(4, 0.1))
dev.off()

combined_plot <- plot_grid(plotlist = c(list(p[[3]],p[[4]],p[[5]] )), ncol = 1, align = "v")
png(file = paste('SHAP Over Time p2', ticker,'.png'), width = 600, height = 500)
plot_grid(combined_plot, rel_widths = c(4, 0.1))
dev.off()

combined_plot <- plot_grid(plotlist = c(list(p[[6]],p[[7]],p[[8]] )), ncol = 1, align = "v")
png(file = paste('SHAP Over Time p3', ticker,'.png'), width = 600, height = 500)
plot_grid(combined_plot, rel_widths = c(4, 0.1))
dev.off()

combined_plot <- plot_grid(plotlist = c(list(p[[9]],p[[10]] )), ncol = 1, align = "v")
png(file = paste('SHAP Over Time p4', ticker,'.png'), width = 600, height = 500)
plot_grid(combined_plot, rel_widths = c(4, 0.1))
dev.off()
