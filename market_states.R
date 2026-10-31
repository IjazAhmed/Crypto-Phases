classify_market_states <- function(file, output_path, row_ranges = c("1-15", "15-45", "45-100"), cols = NULL) {
  
  texfile <- paste0(output_path, "mean_returns.tex")
  excelfile <- paste0(output_path, "mean_returns.xlsx")
  
  # Load required packages
  library(readxl)
  library(dplyr)
  library(xtable)
  library(openxlsx)
  
  
  # Step 1: Read data
  data <- read_excel(file)
  
  # Step 2: Subset columns (if provided) - keep Index if needed for reference
  if (!is.null(cols)) {
    data <- data[, cols, drop = FALSE]
  }
  
  # Step 3: Initialize list to store mean returns for each range
  all_mean_returns <- list()
  
  # Step 4: Loop through each row range, calculate mean returns, and store in list
  for (range in row_ranges) {
    # Parse the range into start and end
    range_split <- strsplit(range, "-")[[1]]
    start_row <- as.integer(range_split[1])
    end_row <- as.integer(range_split[2])
    
    # Subset data for the specified rows (EXCLUDE Index column for calculations)
    subset_data <- data[start_row:end_row, -1, drop = FALSE]  # Remove Index column (column 1)
    
    # Step 5: Compute log returns (use all price columns)
    log_returns <- subset_data %>%
      mutate(across(everything(), ~ c(NA, diff(log(.)))))
    
    # Step 6: Calculate mean returns for the current subset
    mean_returns <- colMeans(log_returns, na.rm = TRUE)
    
    # Store the result in the list with a label for each range
    all_mean_returns[[paste0("Day", range)]] <- mean_returns  # Creates "Day1-30" format
  }
  
  # Step 7: Convert the list of mean returns to a data frame for proper column naming
  all_mean_returns_df <- as.data.frame(all_mean_returns)
  
  # Step 8: Create version for Excel with underscores (to prevent Excel from changing hyphens to dots)
  all_mean_returns_df_excel <- all_mean_returns_df
  colnames(all_mean_returns_df_excel) <- gsub("-", "_", colnames(all_mean_returns_df_excel))  # Day1-30 -> Day1_30
  
  # Step 9: Create LaTeX table with original Day names (keep hyphens)
  latex_table <- xtable(all_mean_returns_df, 
                        caption = "Mean Returns of Selected Assets in Different Day Ranges", 
                        label = "tab:mean_returns")
  
  # Step 10: Write LaTeX table to a .tex file
  print(latex_table, type = "latex", include.rownames = TRUE, file = texfile)
  
  # Step 11: Write to Excel using the underscore version
  write.xlsx(all_mean_returns_df_excel, excelfile, rowNames = TRUE)
  
  # Step 12: Plot with the original data frame (bypassing Excel file reading)
  plot_trend_phases(all_mean_returns_df, output_path)
  
  # Step 13: Perform t-test using the original data frame
  t_test_results <- perform_day_ttest(all_mean_returns_df, output_path)
  
  
  # Return both results
  return(list(mean_returns = all_mean_returns_df, t_test_results = t_test_results))
}

# Function to perform One-Sample T-Test for each Day Range and add interpretation
perform_day_ttest <- function(data, output_path) {
  # Load required package
  library(openxlsx)
  
  # Results for each day range
  results <- data.frame(Day_Range = character(0), mean_value = numeric(0), 
                        t_statistic = numeric(0), p_value = numeric(0), 
                        sentiment = character(0), interpretation = character(0))
  
  # Loop through each day range (all columns since this is the mean returns data)
  for (day_range in colnames(data)) {
    # Perform t-test for each day range (null hypothesis: mean = 0)
    t_test <- t.test(data[[day_range]], mu = 0)
    
    # Calculate the mean of the day range
    mean_val <- mean(data[[day_range]])
    
    # Classify sentiment based on the mean value
    sentiment <- ifelse(mean_val > 0, "Bullish", 
                        ifelse(mean_val < 0, "Bearish", "Sideways"))
    
    # Add results to the dataframe with interpretation
    interpretation <- ifelse(t_test$p.value < 0.05, 
                             ifelse(sentiment == "Bullish", "Significant Bullish", 
                                    ifelse(sentiment == "Bearish", "Significant Bearish", "Significant Sideways")),
                             "Not Significant")
    
    # Append the result to the data frame
    results <- rbind(results, data.frame(
      Day_Range = day_range,
      mean_value = mean_val,
      t_statistic = t_test$statistic,
      p_value = t_test$p.value,
      sentiment = sentiment,
      interpretation = interpretation
    ))
  }
  
  results$mean_value <- sprintf("%.5f", results$mean_value)
  
  # Ensure output directory exists
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  # Save the results to a LaTeX table
  latex_output <- xtable(results)
  print(latex_output, type = "latex", file = paste0(output_path, "t_test_results.tex"))
  
  # Save the results to an Excel file using openxlsx
  write.xlsx(results, paste0(output_path, "t_test_results.xlsx"), rowNames = FALSE)
  
  print(results)
  
  return(results)
}



plot_trend_phases <- function(mean_returns_df, output_path) {
  # MODIFIED: Accept data frame directly instead of Excel file
  
  library(openxlsx)
  library(ggplot2)
  library(reshape2)
  library(dplyr)
  library(tidyr)
  library(ggrepel)
  library(tibble)
  
  # Use the data frame directly (no Excel file reading)
  # Column names are already in "Day1-30" format
  
  # Convert to long format for ggplot
  data_long <- mean_returns_df %>%
    rownames_to_column("Crypto") %>%
    melt(id.vars = "Crypto", variable.name = "Phase", value.name = "MeanReturn") %>%
    mutate(Phase = factor(Phase, levels = colnames(mean_returns_df)),
           Trend = case_when(
             MeanReturn > 0.001 ~ "Bullish",
             MeanReturn < -0.001 ~ "Bearish",
             TRUE ~ "Sideways"
           ),
           Trend = factor(Trend, levels = c("Bullish", "Sideways", "Bearish")))
  
  # Create color palette
  trend_colors <- c("Bullish" = "#2E8B57", "Bearish" = "#DC143C", "Sideways" = "#696969")
  

  
  # Plot 2: Heatmap with Trend Annotations
  p2 <- ggplot(data_long, aes(x = Phase, y = reorder(Crypto, MeanReturn), fill = MeanReturn)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.4f", MeanReturn)), size = 3, color = "black") +
    scale_fill_gradient2(low = "#DC143C", mid = "white", high = "#2E8B57", 
                         midpoint = 0, name = "Mean Return") +
    labs(title = "",
         x = "Market Phase", 
         y = "Cryptocurrency") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  
  ggsave(paste0(output_path, "trend_analysis_heatmap.png"), p2, width = 10, height = 8, dpi = 300)

  
  # Print the plots
 
  print(p2)

  
  return (p2)
}