generate_phase_level_summary <- function(phase_wise_stats_path, output_path) {
  
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
  

  
  volatility_results_df <- data.frame(
    Day_Range = character(),
    volatility_value = numeric(),
    t_statistic = numeric(),
    p_value = numeric(),
    regime = character(),
    interpretation = character(),
    stringsAsFactors = FALSE
  )
  
  # Calculate overall median volatility for regime classification
  all_volatilities <- c(phase_data$`Day1-30-Volatility`, 
                        phase_data$`Day31-60-Volatility`, 
                        phase_data$`Day61-100-Volatility`)
  overall_median_vol <- median(all_volatilities, na.rm = TRUE)
  
  # Analyze each phase
  for (j in 1:nrow(phases)) {
    phase_name <- phases$Name[j]
    mean_col <- phases$Mean_Col[j]
    vol_col <- phases$Vol_Col[j]
    
    # Get all mean returns and volatilities for this phase
    phase_means <- as.numeric(phase_data[[mean_col]])
    phase_vols <- as.numeric(phase_data[[vol_col]])
    
    # Remove NA values
    phase_means <- phase_means[!is.na(phase_means)]
    phase_vols <- phase_vols[!is.na(phase_vols)]
    
    n_assets <- length(phase_means)
    
  
    # --- 1. VOLATILITY ANALYSIS ---
    mean_vol <- mean(phase_vols)
    se_vols <- sd(phase_vols) / sqrt(n_assets)
    t_stat_vols <- mean_vol / se_vols  # Testing H0: μ = 0 (always significant)
    p_val_vols <- 2 * pt(-abs(t_stat_vols), df = n_assets - 1)
    
    # Classify volatility regime
    if (mean_vol > 1.5 * overall_median_vol) {
      regime <- "High"
    } else if (mean_vol < 0.5 * overall_median_vol) {
      regime <- "Low"
    } else {
      regime <- "Moderate"
    }
    
    interpretation_vols <- ifelse(p_val_vols < 0.05, 
                                  paste("Significant", regime),
                                  paste("Not Significant", regime))
    
    volatility_results_df <- rbind(volatility_results_df, data.frame(
      Day_Range = phase_name,
      volatility_value = mean_vol,
      t_statistic = t_stat_vols,
      p_value = p_val_vols,
      regime = regime,
      interpretation = interpretation_vols,
      stringsAsFactors = FALSE
    ))
  }
  
  # --- SAVE RESULTS ---
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  
  # Save Excel files
  write.xlsx(volatility_results_df, paste0(output_path, "phase_level_volatility_summary.xlsx"), rowNames = FALSE)
  
  # Generate LaTeX tables

  
  generate_summary_latex(volatility_results_df, "phase_level_volatility_summary.tex", 
                         "Phase-Level Analysis of Cross-Sectional Average Volatility", 
                         "tab:phase_volatility", output_path, is_volatility = TRUE)
  
  message("✓ Phase-level analysis complete!")
  message("✓ Generated: phase_level_volatility_summary.xlsx/.tex")
  
  return(volatility_summary = volatility_results_df)
}



# UPDATED HELPER FUNCTION FOR LaTeX OUTPUT
generate_summary_latex <- function(data, filename, caption, label, output_path, is_volatility = FALSE) {
  sink(paste0(output_path, filename))
  cat("\\begin{table}[htbp]\n")
  cat("\\centering\n")
  cat("\\caption{", caption, "}\n", sep = "")
  cat("\\label{", label, "}\n", sep = "")
  cat("\\small\n")
  cat("\\begin{tabular}{lrrrll}\n")
  cat("\\toprule\n")
  
  if (is_volatility) {
    cat("\\textbf{Phase} & \\textbf{Volatility} & \\textbf{t-stat} & \\textbf{p-value} & \\textbf{Regime} & \\textbf{Interpretation} \\\\\n")
  } else {
    cat("\\textbf{Phase} & \\textbf{Mean Return} & \\textbf{t-stat} & \\textbf{p-value} & \\textbf{Sentiment} & \\textbf{Interpretation} \\\\\n")
  }
  
  cat("\\midrule\n")
  
  for(i in 1:nrow(data)) {
    row <- data[i, ]
    
    # Format the p-value into proper scientific notation for LaTeX
    p_val_formatted <- format_p_value_for_latex(row$p_value)
    
    if (is_volatility) {
      cat(sprintf("%s & %.5f & %.3f & %s & %s & %s \\\\\n",
                  row$Day_Range, 
                  row$volatility_value, 
                  row$t_statistic, 
                  p_val_formatted, # Use the formatted p-value
                  row$regime, 
                  row$interpretation))
    } else {
      cat(sprintf("%s & %.5f & %.3f & %s & %s & %s \\\\\n",
                  row$Day_Range, 
                  row$mean_value, 
                  row$t_statistic, 
                  p_val_formatted, # Use the formatted p-value
                  row$sentiment, 
                  row$interpretation))
    }
  }
  
  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat("\\end{table}\n")
  sink()
}

# NEW HELPER FUNCTION: Format p-value for LaTeX
format_p_value_for_latex <- function(p_value) {
  if (p_value < 0.001) {
    # Convert to scientific notation and format for LaTeX
    exponent <- floor(log10(p_value))
    coefficient <- p_value / (10^exponent)
    # Round coefficient to 3 significant digits
    coefficient_rounded <- signif(coefficient, digits = 3)
    # Return LaTeX formatted string
    return(sprintf("$%.2f \\times 10^{%d}$", coefficient_rounded, exponent))
  } else {
    # For p-values >= 0.001, show normally with 3 decimal places
    return(sprintf("%.3f", p_value))
  }
}

