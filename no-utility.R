
# Define the helper function
find_class_package <- function(obj) {
  cls <- class(obj)[1]  # get main class
  cat("Class:", cls, "\n")
  cat("Slots available:\n")
  if (isS4(obj)) {
    print(slotNames(obj))
  } else {
    cat("Object is not S4; no slots.\n")
  }
  invisible(cls)
}


final_volatility_verdict_maths <- function(file_path, output_path, k_regimes = 3, seed = 123) {
  library(readxl)
  library(MSwM)
  library(dplyr)
  library(openxlsx)
  library(xtable)
  
  set.seed(seed)  # Fixed seed for reproducibility
  
  latex_file <- paste0(output_path, "verdict_table.tex")
  excel_file <- paste0(output_path, "verdict_table.xlsx")
  sheet <- 1
  
  df <- read_excel(file_path, sheet = sheet)
  df_prices <- df[, -1]
  verdict_table <- data.frame()
  
  legend_str <- if (k_regimes == 3) {
    "s_1 = LowVol, s_2 = ModerateVol, s_3 = HighVol"
  } else if (k_regimes == 2) {
    "s_1 = LowVol, s_2 = HighVol"
  } else {
    paste0("s_", seq_len(k_regimes), " = Unknown")
  }
  
  for (crypto in colnames(df_prices)) {
    price <- as.numeric(df_prices[[crypto]])
    r <- diff(log(price))
    
    t_fraction_str <- NA
    torder_str <- NA
    verdict_label <- NA
    
    if (!(all(is.na(r)) || sd(r, na.rm = TRUE) < 1e-8)) {
      base_model <- lm(r ~ 1)
      ms_model <- tryCatch({
        msmFit(base_model, k = k_regimes, sw = c(FALSE, TRUE), control = list(parallel = FALSE))
      }, error = function(e) NULL)
      
      if (!is.null(ms_model)) {
        smoothed_probs <- ms_model@Fit@smoProb
        states <- apply(smoothed_probs, 1, which.max)
        time_fraction <- table(factor(states, levels = 1:ncol(smoothed_probs))) / length(states)
        t_fraction_str <- paste0("t_", seq_along(time_fraction), " = ", round(time_fraction, 3), collapse = ", ")
        
        ord <- order(-time_fraction)
        frac_vals <- round(time_fraction[ord], 3)
        torder_str <- paste0("T_{(", ord[1], ")}")
        for (j in 2:length(ord)) {
          if (frac_vals[j] == frac_vals[j-1]) {
            torder_str <- paste0(torder_str, " = ", "T_{(", ord[j], ")}")
          } else {
            torder_str <- paste0(torder_str, " > ", "T_{(", ord[j], ")}")
          }
        }
        
        dominant_idx <- which.max(time_fraction)
        if (k_regimes == 2) {
          verdict_label <- paste0("", c("LowVol", "HighVol")[dominant_idx])
        } else if (k_regimes == 3) {
          verdict_label <- paste0("", c("LowVol", "ModerateVol", "HighVol")[dominant_idx])
        } else {
          verdict_label <- paste0("R_{dom} = s_", dominant_idx)
        }
      }
    }
    
    verdict_table <- bind_rows(verdict_table, data.frame(
      Crypto = crypto,
      TimeFraction = t_fraction_str,
      TOrder = torder_str,
      Verdict = verdict_label,
      Legend = legend_str
    ))
  }
  
  verdict_transposed <- rbind(
    TimeFraction = setNames(verdict_table$TimeFraction, verdict_table$Crypto),
    TOrder = setNames(verdict_table$TOrder, verdict_table$Crypto),
    Verdict = setNames(verdict_table$Verdict, verdict_table$Crypto),
    Legend = setNames(verdict_table$Legend, verdict_table$Crypto)
  )
  
  print(xtable(as.data.frame(verdict_transposed)),
        type = "latex", file = latex_file, include.rownames = TRUE)
  
  write.xlsx(as.data.frame(verdict_transposed), excel_file, rowNames = TRUE)
  
  return(verdict_transposed)
}




# Function: final volatility verdict using raw (or standardized) prices
final_volatility_verdict_raw <- function(file_path, sheet = 1, k_regimes = 2, standardize = TRUE) {
  # Step 1: Load data
  df <- read_excel(file_path, sheet = sheet)
  
  # Step 2: Remove first column (date/index)
  df_prices <- df[, -1]
  
  # Step 3: Prepare results table
  verdict_table <- data.frame()
  
  # Step 4: Loop over each crypto
  for (crypto in colnames(df_prices)) {
    price <- as.numeric(df_prices[[crypto]])
    
    # Optionally standardize to reduce scale differences
    if (standardize) {
      price <- (price - mean(price, na.rm = TRUE)) / sd(price, na.rm = TRUE)
    }
    
    # Fit Markov-Switching variance model
    base_model <- lm(price ~ 1)
    
    # Wrap in tryCatch to handle potential singularities
    ms_model <- tryCatch({
      msmFit(base_model, k = k_regimes, sw = c(FALSE, TRUE), control = list(parallel = FALSE))
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(ms_model)) {
      verdict_label <- NA
    } else {
      # Most-likely state per day
      smoothed_probs <- ms_model@Fit@smoProb
      states <- apply(smoothed_probs, 1, which.max)
      
      # Count frequencies
      counts <- table(factor(states, levels = 1:k_regimes))
      
      # Determine dominant regime
      dominant_state <- as.numeric(names(counts)[which.max(counts)])
      verdict_label <- c("NoVolatility", "LowVol", "HighVol")[dominant_state]
    }
    
    # Add to results table
    verdict_table <- bind_rows(verdict_table, data.frame(
      Crypto = crypto,
      Verdict = verdict_label
    ))
  }
  
  return(verdict_table)
}


# Function: classify final volatility verdict for each crypto
final_volatility_verdict <- function(file_path, sheet = 1, k_regimes = 2) {
  # Step 1: Load data
  df <- read_excel(file_path, sheet = sheet)
  
  # Step 2: Remove first column (date/index)
  df_prices <- df[, -1]
  
  # Step 3: Store results
  verdict_table <- data.frame()
  
  # Step 4: Loop over each crypto
  for (crypto in colnames(df_prices)) {
    price <- as.numeric(df_prices[[crypto]])
    
    # Compute log returns
    r <- diff(log(price))
    
    # Fit MS variance model
    base_model <- lm(r ~ 1)
    ms_model <- msmFit(base_model, k = k_regimes, sw = c(FALSE, TRUE), control = list(parallel = FALSE))
    
    # Most-likely states per day
    smoothed_probs <- ms_model@Fit@smoProb
    states <- apply(smoothed_probs, 1, which.max)
    
    # Count frequencies
    counts <- table(factor(states, levels = 1:k_regimes))
    
    # Determine the dominant regime
    dominant_state <- as.numeric(names(counts)[which.max(counts)])
    
    # Map state to verdict label
    verdict_label <- c("NoVolatility", "LowVol", "HighVol")[dominant_state]
    
    # Add to table
    verdict_table <- bind_rows(verdict_table, data.frame(
      Crypto = crypto,
      Verdict = verdict_label
    ))
  }
  
  return(verdict_table)
}


# Function: compute volatility regime table (day-based, across cryptos)
compute_volatility_table <- function(file_path, sheet = 1, k_regimes = 3, phase_length = 15) {
  # Step 1: Load all crypto prices
  df <- read_excel(file_path, sheet = sheet)
  
  # Step 2: Remove the first column (assumed to be Date/Index)
  df <- df[ , -1]
  
  # Step 3: Keep only numeric columns (crypto prices)
  price_mat <- as.matrix(df[, sapply(df, is.numeric)])
  
  # Step 4: Compute log returns for each crypto
  log_ret_mat <- apply(price_mat, 2, function(p) diff(log(p)))
  
  # Step 5: Compute cross-sectional volatility per day
  # (std dev across cryptos on each day)
  R <- apply(log_ret_mat, 1, sd, na.rm = TRUE)
  n <- length(R)
  
  # Step 6: Fit Markov-Switching variance model on daily volatility
  base_model <- lm(R ~ 1)
  ms_model <- msmFit(base_model, k = k_regimes, sw = c(FALSE, TRUE), control = list(parallel = FALSE))
  
  # Smoothed probabilities (n x k_regimes)
  smoothed_probs <- ms_model@Fit@smoProb
  
  # Most-likely state for each day
  states <- apply(smoothed_probs, 1, which.max)
  
  # Step 7: Split into phases
  n_phases <- ceiling(n / phase_length)
  phases <- rep(1:n_phases, each = phase_length)[1:n]
  
  # Step 8: Summarize % per state by phase
  summary_table <- data.frame()
  for (w in 1:n_phases) {
    idx <- which(phases == w)
    n_w <- length(idx)
    counts <- table(factor(states[idx], levels = 1:k_regimes))
    percents <- 100 * counts / n_w
    
    row <- data.frame(
      Phase = paste0("Phase ", w),
      NoVol = percents[1],
      LowVol = percents[2],
      HighVol = percents[3]
    )
    summary_table <- bind_rows(summary_table, row)
  }
  
  return(summary_table)
}





# Define the function
generate_summary_table <- function(file_path, output_file = "summary_table.tex") {
  # Read the data from the specified sheet
  MODDATA <- read_excel(file_path, sheet = "100-days")
  
  # Attach the data
  attach(MODDATA)
  
  # Convert the data into a zoo object (time series)
  MD <- zoo(x = MODDATA[, !grepl("Index", colnames(MODDATA))], order.by = MODDATA$Index)
  
  # Generate summary statistics
  summary_stats <- SummaryStatistics(MD)
  
  # Output the summary statistics in LaTeX table format
  summary_stats_table <- kableExtra::kable(summary_stats, format = "latex", booktabs = TRUE)
  
  # Save the LaTeX table to a .tex file
  writeLines(summary_stats_table, con = output_file)
  
  # Return the LaTeX table code
  return(summary_stats_table)
}


getMeanReturns <- function(file, sheet, row_ranges = c("1-15", "15-45", "45-100"), 
                           cols = NULL, texfile = "mean_returns.tex", excelfile = "mean_returns.xlsx") {
  
  # Step 1: Read data
  data <- read_excel(file, sheet = sheet)
  
  # Step 2: Subset columns (if provided)
  if (!is.null(cols)) {
    # Ensure Index column is always included (assuming it's the first column)
    data <- data[, c(1, cols), drop = FALSE]
  }
  
  # Step 3: Initialize list to store mean returns for each range
  all_mean_returns <- list()
  
  # Step 4: Loop through each row range, calculate mean returns, and store in list
  for (range in row_ranges) {
    # Parse the range into start and end
    range_split <- strsplit(range, "-")[[1]]
    start_row <- as.integer(range_split[1])
    end_row <- as.integer(range_split[2])
    
    # Subset data for the specified rows
    subset_data <- data[start_row:end_row, , drop = FALSE]
    
    # Step 5: Compute log returns
    prices <- subset_data %>% select(-Index)
    
    log_returns <- prices %>%
      mutate(across(everything(), ~ c(NA, diff(log(.)))))
    
    # Step 6: Calculate mean returns for the current subset
    mean_returns <- colMeans(log_returns, na.rm = TRUE)
    
    # Store the result in the list with a label for each range
    # Rename the range to "Day1~15", "Day16~45", etc.
    all_mean_returns[[paste("Day", range, sep = "~")]] <- mean_returns
  }
  
  # Step 7: Convert the list of mean returns to a data frame for proper column naming
  all_mean_returns_df <- as.data.frame(all_mean_returns)
  
  # Step 8: Modify column names for LaTeX output
  colnames(all_mean_returns_df) <- gsub("\\.", "~", colnames(all_mean_returns_df))
  
  # Step 9: Create LaTeX table with custom Day names
  latex_table <- xtable(all_mean_returns_df, 
                        caption = "Mean Returns of Selected Assets in Different Day Ranges", 
                        label = "tab:mean_returns")
  
  # Step 10: Write LaTeX table to a .tex file
  print(latex_table, type = "latex", include.rownames = TRUE, file = texfile)
  
  
  write.xlsx(all_mean_returns_df, excelfile, rowNames = TRUE)
  
  # Return the mean returns
  return(all_mean_returns_df)
}


# Load necessary libraries
library(readxl)
library(dplyr)



# Function to perform One-Sample T-Test for each Day Range and add interpretation
perform_day_ttest <- function(data, output_path) {
  # Results for each day range
  results <- data.frame(Day_Range = character(0), mean_value = numeric(0), 
                        t_statistic = numeric(0), p_value = numeric(0), 
                        sentiment = character(0), interpretation = character(0))
  
  # Loop through each day range (skip the first column which is Crypto name)
  for (day_range in colnames(data)[-1]) {
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
  
  # Save the results to a LaTeX table
  latex_output <- xtable(results)
  print(latex_output, type = "latex", file = paste0(output_path, "/t_test_results.tex"))
  
  # Save the results to an Excel file
  write_xlsx(results, paste0(output_path, "/t_test_results.xlsx"))
  
  return(results)
}












# ===== Flexible Function (Day Index as Integer) =====
plot_crypto_logreturns <- function(file, sheet, rows = NULL, cols = NULL) {
  
  # Step 1: Read data
  data <- read_excel(file, sheet = sheet)
  
  # Step 2: Subset rows (if provided)
  if (!is.null(rows)) {
    data <- data[rows, , drop = FALSE]
  }
  
  # Step 3: Subset columns (if provided, using indices)
  if (!is.null(cols)) {
    # Ensure Index column is always included (assuming it's the first column)
    data <- data[, c(1, cols), drop = FALSE]
  }


  
  # Step 4: Compute log returns
  prices <- data %>% select(-Index)
  
  log_returns <- prices %>%
    mutate(across(everything(), ~ c(NA, diff(log(.)))))
  
  # Keep original Index as integers
  log_returns <- bind_cols(Index = data$Index, log_returns)
  
  # Step 5: Convert to long format
  log_returns_long <- log_returns %>%
    pivot_longer(-Index, names_to = "Crypto", values_to = "LogReturn")
  
  # Step 6: Interactive Plot with Plotly
  fig <- plot_ly(
    data = log_returns_long,
    x = ~Index,           # Use integer day index
    y = ~LogReturn,
    color = ~Crypto,
    colors = "Set2",
    type = 'scatter',
    mode = 'lines',
    text = ~paste("Crypto:", Crypto,
                  "<br>Day:", Index,
                  "<br>LogReturn:", round(LogReturn, 6)),
    hoverinfo = 'text'
  )
  
  fig <- fig %>%
    layout(
      title = "Log Returns of Cryptocurrencies",
      xaxis = list(title = "Day Index", dtick = 1), # ensures integer ticks
      yaxis = list(title = "Log Return")
    )
  
  
  return(fig)
}


# ===== Flexible Function (Plot Actual Prices) =====
plot_crypto_prices <- function(file, sheet, rows = NULL, cols = NULL) {
  
  # Step 1: Read data
  data <- read_excel(file, sheet = sheet)
  
  # Step 2: Subset rows (if provided)
  if (!is.null(rows)) {
    data <- data[rows, , drop = FALSE]
  }
  
  # Step 3: Subset columns (if provided, using indices)
  if (!is.null(cols)) {
    # Ensure Index column is always included (assuming it's the first column)
    data <- data[, c(1, cols), drop = FALSE]
  }
  
  # Step 4: Extract prices (keep Index)
  prices <- data %>% select(-Index)
  
  # Step 5: Convert to long format
  prices_long <- bind_cols(Index = data$Index, prices) %>%
    pivot_longer(-Index, names_to = "Crypto", values_to = "Price")
  
  # Step 6: Interactive Plot with Plotly
  fig <- plot_ly(
    data = prices_long,
    x = ~Index,          # Use integer day index
    y = ~Price,
    color = ~Crypto,
    colors = "Set2",
    type = 'scatter',
    mode = 'lines',
    text = ~paste("Crypto:", Crypto,
                  "<br>Day:", Index,
                  "<br>Price:", Price),
    hoverinfo = 'text'
  )
  
  fig <- fig %>%
    layout(
      title = "Cryptocurrency Prices",
      xaxis = list(title = "Day Index", dtick = 1),  # integer ticks
      yaxis = list(title = "Price")
    )
  
  return(fig)
}





# ===== Example Usage =====
# Use all data
# plot_crypto_logreturns("your_file.xlsx", "100-days")

# Use only rows 2 to 20 and selected columns (BTC, ETH, DOGE)
# plot_crypto_logreturns("your_file.xlsx", "100-days", rows = 2:20, cols = c("BTC", "ETH", "DOGE"))

# ===== Example Usage =====
# fig <- plot_crypto_logreturns("your_file.xlsx", "100-days")
# fig


# Function to compute log returns and save into 3 sheets
save_log_returns <- function(input_file, sheet_name = "100-days", output_file = "crypto_log_returns_splits.xlsx") {
  
  # 1. Load sheet
  data_raw <- read_excel(input_file, sheet = sheet_name)
  
  # 2. Remove Start_Date and End_Date rows
  crypto_data <- data_raw %>% 
    slice(-c(1, 2))
  
  # 3. Ensure numeric values
  crypto_data <- crypto_data %>% 
    mutate(across(-Index, as.numeric))
  
  # 4. Compute log returns
  log_returns <- crypto_data %>%
    mutate(across(-Index, ~ c(NA, diff(log(.)))))
  
  # 5. Split into sub-periods
  log_returns_15 <- log_returns %>% slice(1:4)
  log_returns_35 <- log_returns %>% slice(16:50)
  log_returns_50 <- log_returns %>% slice(51:100)
  
  # 6. Write to Excel with 3 sheets
  write.xlsx(
    list("15-days" = log_returns_15,
         "35-days" = log_returns_35,
         "50-days" = log_returns_50),
    file = output_file,
    rowNames = FALSE
  )
  
  message("✅ Log returns saved to: ", output_file)
}

# Example usage:
# save_log_returns("your_file.xlsx", sheet_name = "100-days", output_file = "crypto_log_returns_splits.xlsx")



read_excel_except <- function(file_path, exclude = "Sheet1", by_position = FALSE, combine = TRUE) {
  # Get all sheet names
  all_sheets <- excel_sheets(file_path)
  
  # Exclude either by name or by position
  if (by_position) {
    sheets_to_read <- all_sheets[-exclude]   # exclude by index (e.g. 1 for first sheet)
  } else {
    sheets_to_read <- setdiff(all_sheets, exclude)
  }
  
  # Read all selected sheets into a named list
  data_list <- map(sheets_to_read, ~ read_excel(file_path, sheet = .x)) %>%
    set_names(sheets_to_read)
  
  # Return list or combined dataframe
  if (combine) {
    return(bind_rows(data_list, .id = "sheet_name"))
  } else {
    return(data_list)
  }
}

# ----------------------------------------------
# Utility Functions for TVP-VAR Network Analysis
# ----------------------------------------------

# Write a DataFrame to an Excel file
writeToExcel <- function(data, path, name){
  name_path <- file.path(path, name)
  write_xlsx(data, name_path)
}

# Clear R memory: removes all variables, clears plots and console
clearMemory <- function (){
  rm(list = ls())            # remove all objects
  gc()                       # garbage collection
  cat("\014")                # clear console
  while (!is.null(dev.list())) dev.off()  # close all plots
}

# Read Excel and calculate log returns (excluding Date column)
getReturns <- function(data){
  data <- read_excel(path)
  
  # Extract the Date column (assumed to be the first column)
  #date_col <- data[[1]]
  
  # Extract only numeric data (excluding Date)
  #numeric_data <- data[, -1]
  
  # Calculate log returns
  data_returns <- data.frame(
    sapply(data, function(x) c(NA, diff(log(x))))
  )
  
  
  # Keep same column names
  colnames(returns) <- paste0(colnames(numeric_data), "")
  
  # Combine Date with return data
  data_return <- data.frame(Date = date_col, returns)
  data_return <- na.omit(data_return)
  return(data_return)
  
  
}

# Convert Excel to LaTeX table using xtable
excel_to_latex <- function(excel_path, output_path, include_rownames = FALSE) {
  df <- read_excel(excel_path)
  
  # Generate LaTeX table
  raw_table <- print(
    xtable(df),
    type = "latex",
    include.rownames = include_rownames,
    print.results = FALSE
  )
  
  # Write to .tex file
  if (!is.null(output_path)) {
    writeLines(raw_table, output_path)
  }
}

# Run network analysis using ConnectednessApproach on selected columns
networkAnalysis <- function(data_return, selected_columns, output_path, file_name){
  data_return_zoo <- zoo(data_return[, selected_columns], order.by = data_return$Date)
  
  result <- ConnectednessApproach(
    data_return_zoo,
    nlag = 1,
    nfore = 5,
    window.size = 50,
    model = "TVP-VAR"
  )
  
  make_Excel_Latex_File(result, output_path, file_name)
  PlotNetwork(result, method = "NPDC", threshold = 0.05)
  PlotTCI(result)
}

# Export connectedness result to Excel and LaTeX
make_Excel_Latex_File <- function(result, path, file_name){
  df <- as.data.frame(result$TABLE)
  df$Variable <- rownames(df)
  df <- df[, c("Variable", setdiff(names(df), "Variable"))]
  
  excel_file_path <- paste0(path, "/", file_name, ".xlsx")
  write_xlsx(df, excel_file_path)
  
  latex_file_path <- paste0(path, "/", file_name, ".tex")
  excel_to_latex(excel_file_path, latex_file_path)
}

# Generate descriptive statistics and stationarity tests
generate_descriptive_stat <- function(data, output_path){
  asset_data <- data[, -1]  # Drop Date column
  
  # Function to compute stats and stationarity tests per asset
  summary_stats <- function(x) {
    
    x <- na.omit(x)
    
    # Convert to numeric if possible
    x <- as.numeric(x)
    
    # Handle case where all values were NA or non-numeric
    if (length(x) == 0 || all(is.na(x))) {
      return(NA)  # or return a list of NAs for consistency
    }
    
    
    adf_test <- ur.df(x, type = "drift", selectlags = "AIC")
    adf_stat <- adf_test@teststat[1]
    adf_crit  <- adf_test@cval[1, "5pct"]
    adf_result <- ifelse(adf_stat < adf_crit, "Stationary", "Non-Stationary")
    
    kpss_test <- ur.kpss(x, type = "mu", use.lag = NULL)
    kpss_stat <- kpss_test@teststat
    kpss_crit <- kpss_test@cval["5pct"]
    kpss_result <- ifelse(kpss_stat < kpss_crit, "Stationary", "Non-Stationary")
    
    c(
      Mean = mean(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      Min = min(x, na.rm = TRUE),
      Max = max(x, na.rm = TRUE),
      Skewness = skewness(x, na.rm = TRUE),
      Kurtosis = kurtosis(x, na.rm = TRUE),
      JB_pvalue = jarque.bera.test(x)$p.value,
      ADF_Stat = round(adf_stat, 3),
      KPSS_Stat = round(kpss_stat, 3)
    )
  }
  
  # Apply to all asset columns
  result <- as.data.frame(t(sapply(asset_data, summary_stats)))
  result <- cbind(Asset = rownames(result), result)
  rownames(result) <- NULL
  
  # Save output to Excel and LaTeX
  writexl::write_xlsx(result, paste0(output_path, "/summary_stat.xlsx"))
  excel_to_latex(paste0(output_path, "/summary_stat.xlsx"), paste0(output_path, "/summary_stat.tex"))
}

