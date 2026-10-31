generate_ttest_and_volatility_tables <- function(phase_wise_stats_path, output_path) {
  
  # Load required libraries
  library(readxl)
  library(dplyr)
  library(openxlsx)
  
  # Read the phase-wise data
  phase_data <- read_excel(phase_wise_stats_path)
  
  # Define phases and their column names
  phases <- data.frame(
    Name = c("Day1-30", "Day31-60", "Day61-100"),
    Mean_Col = c("Day1-30-Mean", "Day31-60-Mean", "Day61-100-Mean"),
    Vol_Col = c("Day1-30-Volatility", "Day31-60-Volatility", "Day61-100-Volatility")
  )
  
  # --- 1. CREATE RETURN SENTIMENT TABLE (T-TESTS) ---
  return_results_df <- data.frame(
    Asset = character(),
    Phase = character(),
    Mean_Return = numeric(),
    t_statistic = numeric(),
    p_value = numeric(),
    Interpretation = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(phase_data)) {
    asset <- phase_data$Asset[i]
    
    for (j in 1:nrow(phases)) {
      phase_name <- phases$Name[j]
      mean_val <- as.numeric(phase_data[i, phases$Mean_Col[j]])
      vol_val <- as.numeric(phase_data[i, phases$Vol_Col[j]])
      
      # Perform t-test calculation (H0: μ = 0)
      n <- 30 # Assuming 30 days per phase
      se <- vol_val / sqrt(n)
      t_stat <- mean_val / se
      p_val <- 2 * pt(-abs(t_stat), df = n-1) # Two-tailed test
      
      # Determine interpretation
      if (p_val < 0.05) {
        interpretation <- ifelse(mean_val > 0, "Significant Bullish", "Significant Bearish")
      } else {
        interpretation <- "Not Significant"
      }
      
      # Append to results
      return_results_df <- rbind(return_results_df, data.frame(
        Asset = asset,
        Phase = phase_name,
        Mean_Return = mean_val,
        t_statistic = t_stat,
        p_value = p_val,
        Interpretation = interpretation,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # --- 2. CREATE VOLATILITY REGIME TABLE ---
  volatility_results_df <- data.frame(
    Asset = character(),
    Phase = character(),
    Volatility = numeric(),
    Classification = character(),
    stringsAsFactors = FALSE
  )
  
  # Calculate benchmark volatility (median) for each phase
  for (j in 1:nrow(phases)) {
    phase_name <- phases$Name[j]
    vol_col <- phases$Vol_Col[j]
    phase_volatilities <- as.numeric(phase_data[[vol_col]])
    median_vol <- median(phase_volatilities, na.rm = TRUE)
    
    for (i in 1:nrow(phase_data)) {
      asset <- phase_data$Asset[i]
      vol_val <- as.numeric(phase_data[i, vol_col])
      
      # Classify volatility based on median
      if (vol_val > 1.5 * median_vol) {
        classification <- "High"
      } else if (vol_val < 0.5 * median_vol) {
        classification <- "Low"
      } else {
        classification <- "Moderate"
      }
      
      volatility_results_df <- rbind(volatility_results_df, data.frame(
        Asset = asset,
        Phase = phase_name,
        Volatility = vol_val,
        Classification = classification,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # --- SAVE RESULTS ---
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  
  # Save Excel files
  write.xlsx(return_results_df, paste0(output_path, "return_t_test_results.xlsx"), rowNames = FALSE)
  write.xlsx(volatility_results_df, paste0(output_path, "volatility_classification.xlsx"), rowNames = FALSE)
  
  # Generate LaTeX tables
  generate_latex_table(return_results_df, "return_t_test_results.tex", "Statistical Significance of Phase-Wise Mean Returns (One-Sample t-Test)", "tab:return_tests", output_path)
  generate_latex_table(volatility_results_df, "volatility_classification.tex", "Volatility Regime Classification by Phase", "tab:volatility_regimes", output_path)
  
  message("✓ Analysis complete! Two tables generated:")
  message("  1. return_t_test_results.xlsx/.tex")
  message("  2. volatility_classification.xlsx/.tex")
  
  return(list(return_results = return_results_df, volatility_results = volatility_results_df))
}

# Helper function to generate LaTeX tables
generate_latex_table <- function(data, filename, caption, label, output_path) {
  sink(paste0(output_path, filename))
  cat("\\begin{table}[htbp]\n")
  cat("\\centering\n")
  cat("\\caption{", caption, "}\n", sep = "")
  cat("\\label{", label, "}\n", sep = "")
  cat("\\small\n")
  
  # Determine table format based on data
  if ("t_statistic" %in% colnames(data)) {
    cat("\\begin{tabular}{llrrrl}\n")
    cat("\\toprule\n")
    cat("\\textbf{Asset} & \\textbf{Phase} & \\textbf{Mean} & \\textbf{t-stat} & \\textbf{p-value} & \\textbf{Interpretation} \\\\\n")
  } else {
    cat("\\begin{tabular}{llrc}\n")
    cat("\\toprule\n")
    cat("\\textbf{Asset} & \\textbf{Phase} & \\textbf{Volatility} & \\textbf{Classification} \\\\\n")
  }
  
  cat("\\midrule\n")
  
  for(i in 1:nrow(data)) {
    row <- data[i, ]
    if ("t_statistic" %in% colnames(data)) {
      cat(sprintf("%s & %s & %.5f & %.3f & %.3f & %s \\\\\n",
                  row$Asset, row$Phase, row$Mean_Return,
                  row$t_statistic, row$p_value, row$Interpretation))
    } else {
      cat(sprintf("%s & %s & %.5f & %s \\\\\n",
                  row$Asset, row$Phase, row$Volatility, row$Classification))
    }
  }
  
  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
  sink()
}

# --- RUN THE FUNCTION ---
results <- generate_ttest_and_volatility_tables(
  phase_wise_stats_path = "/Users/sm25113/100days/results/phase_wise_stats.xlsx",
  output_path = "/Users/sm25113/100days/results/"
)