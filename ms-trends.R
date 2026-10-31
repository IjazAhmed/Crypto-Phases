Markove_Switching_Model_trends <- function(file_path, output_path, k_regimes = 3, seed = 123,
                                    row_ranges = c("1-15", "15-45", "45-60", "60-75", "75-100")) {
  
  set.seed(seed) # Fixed seed for reproducibility
  
  # Load data once
  df <- read_excel(file_path, sheet = 1)
  df_prices_full <- df[, -1]  # Remove first column (date/index)
  
  # Updated legend string for trend detection
  legend_str <- if (k_regimes == 3) {
    "s_1 = Bullish, s_2 = Sideways, s_3 = Bearish"
  } else if (k_regimes == 2) {
    "s_1 = Bullish, s_2 = Bearish"
  } else {
    paste0("s_", seq_len(k_regimes), " = Trend_State")
  }
  
  # Loop over row ranges
  for (range_str in row_ranges) {
    # Parse start and end rows
    bounds <- as.numeric(strsplit(range_str, "-")[[1]])
    start_row <- bounds[1]
    end_row <- bounds[2]
    
    df_prices <- df_prices_full[start_row:end_row, , drop = FALSE]
    verdict_table <- data.frame()
    
    # Loop over cryptos
    for (crypto in colnames(df_prices)) {
      price <- as.numeric(df_prices[[crypto]])
      r <- diff(log(price))
      
      t_fraction_str <- NA
      torder_str <- NA
      verdict_label <- NA
      mean_returns <- rep(NA, k_regimes)  # Store mean returns per regime
      
      if (!(all(is.na(r)) || sd(r, na.rm = TRUE) < 1e-8)) {
        base_model <- lm(r ~ 1)
        ms_model <- tryCatch({
          # Allow both mean and variance to switch for trend detection
          msmFit(base_model, k = k_regimes, sw = c(TRUE, TRUE), control = list(parallel = FALSE))
        }, error = function(e) NULL)
        
        if (!is.null(ms_model)) {
          smoothed_probs <- ms_model@Fit@smoProb
          states <- apply(smoothed_probs, 1, which.max)
          
          # Calculate regime statistics including mean returns
          regime_stats <- lapply(1:k_regimes, function(i) {
            returns_in_regime <- r[states == i]
            list(
              mean_return = mean(returns_in_regime, na.rm = TRUE),
              volatility = sd(returns_in_regime, na.rm = TRUE),
              time_fraction = sum(states == i) / length(states)
            )
          })
          
          # Time fraction per regime
          time_fraction <- sapply(regime_stats, function(x) x$time_fraction)
          t_fraction_str <- paste0("t_", seq_along(time_fraction), " = ", round(time_fraction, 3), collapse = ", ")
          
          # Store mean returns for each regime
          mean_returns <- sapply(regime_stats, function(x) x$mean_return)
          
          # TOrder
          ord <- order(-time_fraction)
          frac_vals <- round(time_fraction[ord], 3)
          torder_str <- paste0("t_", ord[1])
          for (j in 2:length(ord)) {
            if (frac_vals[j] == frac_vals[j - 1]) {
              torder_str <- paste0(torder_str, " = ", "t_", ord[j])
            } else {
              torder_str <- paste0(torder_str, " > ", "t_", ord[j])
            }
          }
          
          # Determine trend-based verdict
          dominant_idx <- which.max(time_fraction)
          dominant_mean_return <- mean_returns[dominant_idx]
          
          # Classify based on mean return of dominant regime
          if (dominant_mean_return > 0.001) {
            verdict_label <- "Bullish"
          } else if (dominant_mean_return < -0.001) {
            verdict_label <- "Bearish"
          } else {
            verdict_label <- "Sideways"
          }
        }
      }
      
      verdict_table <- bind_rows(verdict_table, data.frame(
        Crypto = crypto,
        TimeFraction = t_fraction_str,
        MeanReturns = paste(round(mean_returns, 5), collapse = ", "),
        TOrder = torder_str,
        Verdict = verdict_label
      ))
    }
    
    # ---- Save LaTeX ----
    latex_file <- paste0(output_path, "verdict_table_", range_str, ".tex")
    sink(latex_file)
    print(
      xtable(verdict_table),
      type = "latex",
      include.rownames = FALSE,
      include.colnames = TRUE
    )
    cat(paste0("\n\\vspace{2mm}\n\\footnotesize Legend: ", legend_str, ".\n"))
    sink()
    
    # ---- Save Excel ----
    excel_file <- paste0(output_path, "verdict_table_", range_str, ".xlsx")
    verdict_excel <- bind_rows(
      verdict_table,
      data.frame(Crypto = "Legend", TimeFraction = legend_str, MeanReturns = NA, TOrder = NA, Verdict = NA)
    )
    write.xlsx(verdict_excel, excel_file, rowNames = FALSE)
    
    # ---- Plot ternary ----
    plot_ternary(verdict_table, output_path, k_regimes = k_regimes, range_label = range_str)
  }
}

# --- Helper function (unchanged) ---
checkCordinates <- function(df_row, crypto_name) {
  modified <- FALSE
  new_row <- df_row
  
  if(new_row$t_1 <= 0.01) { 
    new_row$t_1 <- 0.02
    modified <- TRUE
  }
  if(new_row$t_2 <= 0.01) { 
    new_row$t_2 <- 0.02
    modified <- TRUE
  }
  if(new_row$t_3 <= 0.01) { 
    new_row$t_3 <- 0.02
    modified <- TRUE
  }
  
  s <- new_row$t_1 + new_row$t_2 + new_row$t_3
  new_row$t_1 <- new_row$t_1 / s
  new_row$t_2 <- new_row$t_2 / s
  new_row$t_3 <- new_row$t_3 / s
  
  if(modified) {
    message(sprintf("Adjusted coordinates for %s: (%.3f, %.3f, %.3f)",
                    crypto_name, new_row$t_1, new_row$t_2, new_row$t_3))
  }
  
  return(new_row)
}

# --- Modified plotting function for trend detection ---
plot_ternary <- function(verdict_table, output_path, k_regimes = 3,
                         nudge_x = 0.12, nudge_y = 0.05, nudge_z = 0,
                         range_label = NULL) {
  library(ggtern)
  
  # --- Prepare numeric fractions ---
  time_fraction_list <- lapply(verdict_table$TimeFraction, function(x) {
    if (is.na(x)) return(NULL)
    parts <- strsplit(as.character(x), ", ")[[1]]
    vals <- sapply(parts, function(p) as.numeric(sub(".*=", "", p)))
    if (length(vals) != k_regimes) return(NULL)
    vals <- vals / sum(vals)
    return(vals)
  })
  
  valid_idx <- which(!sapply(time_fraction_list, is.null))
  if(length(valid_idx) == 0){
    warning("No valid rows for ternary plot.")
    return(NULL)
  }
  
  time_fraction_df <- do.call(rbind, time_fraction_list[valid_idx])
  colnames(time_fraction_df) <- paste0("t_", 1:k_regimes)
  time_fraction_df <- as.data.frame(time_fraction_df)
  time_fraction_df$Crypto <- verdict_table$Crypto[valid_idx]
  time_fraction_df$Verdict <- verdict_table$Verdict[valid_idx]
  
  # --- Apply checkCordinates for boundary adjustment ---
  adjusted <- do.call(rbind, lapply(seq_len(nrow(time_fraction_df)), function(i) {
    checkCordinates(time_fraction_df[i, ], time_fraction_df$Crypto[i])
  }))
  
  time_fraction_df$t_1_plot <- adjusted$t_1
  time_fraction_df$t_2_plot <- adjusted$t_2
  time_fraction_df$t_3_plot <- adjusted$t_3
  
  # --- Prepare label coordinates ---
  time_fraction_df$label_x <- time_fraction_df$t_1_plot + nudge_x
  time_fraction_df$label_y <- time_fraction_df$t_2_plot + nudge_y
  
  # --- Updated color mapping for trends ---
  trend_colors <- c("Bullish" = "green", "Sideways" = "orange", "Bearish" = "red")
  other_labels <- setdiff(unique(time_fraction_df$Verdict), names(trend_colors))
  if(length(other_labels) > 0){
    trend_colors <- c(trend_colors, setNames(rainbow(length(other_labels)), other_labels))
  }
  
  # --- Base plot ---
  p <- ggtern(data = time_fraction_df,
              aes(x = t_1_plot, y = t_2_plot, z = t_3_plot, color = Verdict)) +
    geom_point(size = 3) +
    scale_color_manual(values = trend_colors) +
    labs(title = paste0("Trend Analysis - Market Regimes: ", range_label),
         x = "Regime 1", y = "Regime 2", z = "Regime 3",
         color = "Market Trend") +
    theme_bw()
  
  # --- Add labels ---
  p <- p + geom_text(aes(x = label_x, y = label_y, label = Crypto),
                     color = "black", size = 3, hjust = 0, vjust = 0)
  
  # --- Save file ---
  plot_file <- "trend_analysis_plot.png"
  if(!is.null(range_label)) plot_file <- paste0("trend_analysis_", range_label, ".png")
  
  ggsave(filename = file.path(output_path, plot_file),
         plot = p, width = 8, height = 6)
  
  print(p)
  invisible(p)
}