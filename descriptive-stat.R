generate_complete_descriptive_tables <- function(first_100_path, output_path) {
  
  # Load required libraries
  library(readxl)
  library(dplyr)
  library(openxlsx)
  library(xtable)
  library(zoo)
  library(moments)
  
  # Create output directory
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  
  # Read and prepare data
  initial_data <- read_excel(first_100_path)
  initial_prices <- initial_data[, -1]
  crypto_names <- colnames(initial_prices)
  
  # Create zoo object for overall stats
  MD <- zoo(x = initial_prices, order.by = initial_data$Index)
  
  # --- 1. GENERATE FOCUSED OVERALL STATISTICS ---
  message("Generating focused overall statistics...")
  
  # Custom Jarque-Bera test function
  jarque_bera_test <- function(x) {
    x <- na.omit(x)
    n <- length(x)
    if (n < 3) return(NA)
    
    m1 <- mean(x)
    m2 <- mean((x - m1)^2)
    m3 <- mean((x - m1)^3)
    m4 <- mean((x - m1)^4)
    
    skewness <- m3 / (m2^(3/2))
    kurtosis <- m4 / (m2^2)
    
    jb <- n * (skewness^2 / 6 + (kurtosis - 3)^2 / 24)
    p_value <- pchisq(jb, df = 2, lower.tail = FALSE)
    
    return(p_value)
  }
  
  # Calculate the 5 essential metrics
  focused_stats <- data.frame(
    Asset = crypto_names,
    Mean = numeric(length(crypto_names)),
    Std.Dev = numeric(length(crypto_names)),
    Skewness = numeric(length(crypto_names)),
    Kurtosis = numeric(length(crypto_names)),
    JB.p.value = numeric(length(crypto_names)),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:length(crypto_names)) {
    asset_data <- na.omit(MD[, i])
    
    if (length(asset_data) > 0) {
      focused_stats$Mean[i] <- mean(asset_data)
      focused_stats$Std.Dev[i] <- sd(asset_data)
      focused_stats$Skewness[i] <- moments::skewness(asset_data)
      focused_stats$Kurtosis[i] <- moments::kurtosis(asset_data)
      focused_stats$JB.p.value[i] <- jarque_bera_test(asset_data)
    }
  }
  
  # Save overall stats to Excel
  write.xlsx(focused_stats, paste0(output_path, "overall_focused_stats.xlsx"), rowNames = FALSE)
  
  # Generate LaTeX for overall stats
  sink(paste0(output_path, "overall_focused_table.tex"))
  cat("\\begin{table}[htbp]\n")
  cat("\\centering\n")
  cat("\\caption{Focused Descriptive Statistics of Cryptocurrency Returns (First 100 Days)}\n")
  cat("\\label{tab:overall_stats}\n")
  cat("\\small\n")
  cat("\\begin{tabular}{lccccc}\n")
  cat("\\toprule\n")
  cat("\\textbf{Asset} & \\textbf{Mean} & \\textbf{Std. Dev.} & \\textbf{Skewness} & \\textbf{Kurtosis} & \\textbf{JB p-value} \\\\\n")
  cat("\\midrule\n")
  
  for(i in 1:nrow(focused_stats)) {
    row <- focused_stats[i, ]
    cat(sprintf("%s & %.5f & %.5f & %.3f & %.3f & %.4f \\\\\n",
                row$Asset, row$Mean, row$Std.Dev, 
                row$Skewness, row$Kurtosis, row$JB.p.value))
  }
  
  cat("\\midrule\n")
  cat("\\multicolumn{6}{l}{\\footnotesize \\textit{Note: JB p-value is for the Jarque-Bera test of normality.}}\\\\\n")
  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
  sink()
  
  # --- 2. GENERATE PHASE-WISE TABLE ---
  message("Generating phase-wise statistics...")
  
  row_ranges <- c("1-30", "31-60", "61-100")
  phase_labels <- c("Day1-30", "Day31-60", "Day61-100")
  
  # Initialize clean data frame
  phase_data <- data.frame(Asset = crypto_names)
  
  for (i in seq_along(row_ranges)) {
    range <- row_ranges[i]
    phase_label <- phase_labels[i]
    
    bounds <- as.numeric(strsplit(range, "-")[[1]])
    start_row <- bounds[1]
    end_row <- bounds[2]
    
    # Extract prices for this phase
    phase_prices <- initial_prices[start_row:end_row, ]
    
    # Calculate Log Returns
    log_returns <- phase_prices %>%
      mutate(across(everything(), ~ c(NA, diff(log(.)))))
    
    # Calculate Mean and Volatility for each crypto
    means <- log_returns %>% summarise(across(everything(), ~mean(., na.rm = TRUE)))
    vols <- log_returns %>% summarise(across(everything(), ~sd(., na.rm = TRUE)))
    
    # Add to data frame
    phase_data[[paste0(phase_label, "-Mean")]] <- as.numeric(means[1, ])
    phase_data[[paste0(phase_label, "-Volatility")]] <- as.numeric(vols[1, ])
  }
  
  # Save phase-wise data to Excel
  write.xlsx(phase_data, paste0(output_path, "phase_wise_stats.xlsx"), rowNames = FALSE)
  
  # Generate LaTeX for phase-wise table
  sink(paste0(output_path, "phase_wise_table.tex"))
  cat("\\begin{table}[htbp]\n")
  cat("\\centering\n")
  cat("\\caption{Phase-Wise Analysis of Returns and Volatility (First 100 Days)}\n")
  cat("\\label{tab:phase_stats}\n")
  cat("\\small\n")
  cat("\\begin{tabular}{lcccccc}\n")
  cat("\\toprule\n")
  cat(" & \\multicolumn{2}{c}{\\textbf{Day 1-30}} & \\multicolumn{2}{c}{\\textbf{Day 31-60}} & \\multicolumn{2}{c}{\\textbf{Day 61-100}} \\\\\n")
  cat("\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}\n")
  cat("\\textbf{Asset} & \\textbf{Mean} & \\textbf{Vol.} & \\textbf{Mean} & \\textbf{Vol.} & \\textbf{Mean} & \\textbf{Vol.} \\\\\n")
  cat("\\midrule\n")
  
  for(i in 1:nrow(phase_data)) {
    row <- phase_data[i, ]
    cat(sprintf("%s & %.5f & %.5f & %.5f & %.5f & %.5f & %.5f \\\\\n",
                row$Asset, 
                row$`Day1-30-Mean`, row$`Day1-30-Volatility`,
                row$`Day31-60-Mean`, row$`Day31-60-Volatility`, 
                row$`Day61-100-Mean`, row$`Day61-100-Volatility`))
  }
  
  cat("\\midrule\n")
  cat("\\multicolumn{7}{l}{\\footnotesize \\textit{Note: Vol. = Volatility (Standard Deviation).}}\\\\\n")
  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
  sink()
  
  message("✓ Complete descriptive analysis finished!")
  message("✓ Generated files:")
  message("  1. overall_focused_stats.xlsx")
  message("  2. overall_focused_table.tex")
  message("  3. phase_wise_stats.xlsx")
  message("  4. phase_wise_table.tex")
  
  return(list(overall_stats = focused_stats, phase_stats = phase_data))
}

