library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(ggtern)
library(xtable)

# ---------------------------
# 1. Load data
# ---------------------------
load_crypto_data <- function(file_path) {
  df <- read_excel(file_path, sheet = 1)
  df[, -1]  # remove first column (date/index)
}

# ---------------------------
# 2. Compute peaks with custom quantiles
# ---------------------------
compute_peaks_custom_quantile <- function(price, peak_quantiles = c(0.5, 0.8, 0.95)) {
  if(all(is.na(price)) || sd(price, na.rm = TRUE) < 1e-8) return(NULL)
  
  abs_log_r <- abs(diff(log(price)))
  
  # Find all peaks
  all_peaks <- findpeaks(abs_log_r, minpeakdistance = 1, sortstr = TRUE)
  if(is.null(all_peaks)) return(NULL)
  peak_idx <- all_peaks[,2]
  
  peak_values <- round(abs_log_r[peak_idx], 5)
  
  # Compute thresholds
  thresholds <- quantile(abs_log_r, probs = sort(peak_quantiles), na.rm = TRUE)
  
  peak_labels <- sapply(peak_values, function(x) {
    if(x < thresholds[1]) {
      "Low Peak"
    } else if(x < thresholds[2]) {
      "Moderate Peak"
    } else {
      "High Peak"
    }
  })
  
  data.frame(PeakValue = peak_values, PeakLabel = as.character(peak_labels))
}

# ---------------------------
# 3. Summarize peaks per crypto
# ---------------------------
summarize_peaks <- function(df_prices, peak_quantiles = c(0.5,0.8,0.95)) {
  summary_table <- data.frame()
  ternary_data <- data.frame(Crypto = character(), Low = numeric(), Moderate = numeric(), High = numeric(), MaxPeakValue = numeric())
  
  total_days <- nrow(df_prices)
  
  for(crypto in colnames(df_prices)) {
    price <- as.numeric(df_prices[[crypto]])
    peaks <- compute_peaks_custom_quantile(price, peak_quantiles)
    
    if(!is.null(peaks)) {
      counts <- table(factor(peaks$PeakLabel, levels = c("Low Peak","Moderate Peak","High Peak")))
      counts <- as.list(counts)
      counts <- lapply(counts, function(x) if(is.null(x)) 0 else x)
      
      num_non_peak <- total_days - sum(unlist(counts))
      dominant <- names(which.max(unlist(counts)))
      max_peak <- max(peaks$PeakValue)
      
      total_peaks <- sum(unlist(counts))
      
      summary_table <- bind_rows(summary_table, 
                                 data.frame(
                                   Crypto = crypto,
                                   TotalDays = total_days,
                                   NumHighPeaks = counts[["High Peak"]],
                                   NumModeratePeaks = counts[["Moderate Peak"]],
                                   NumLowPeaks = counts[["Low Peak"]],
                                   NumNonPeaks = num_non_peak,
                                   MaxPeakValue = max_peak,
                                   FinalVerdict = dominant
                                 ))
      
      if (total_peaks > 0) {
        low_prop <- counts[["Low Peak"]] / total_peaks
        mod_prop <- counts[["Moderate Peak"]] / total_peaks
        high_prop <- counts[["High Peak"]] / total_peaks
      } else {
        low_prop <- 0; mod_prop <- 0; high_prop <- 0
      }
      
      ternary_data <- bind_rows(ternary_data,
                                data.frame(
                                  Crypto = crypto,
                                  Low = low_prop,
                                  Moderate = mod_prop,
                                  High = high_prop,
                                  MaxPeakValue = max_peak
                                ))
    } else {
      summary_table <- bind_rows(summary_table,
                                 data.frame(
                                   Crypto = crypto,
                                   TotalDays = total_days,
                                   NumHighPeaks = 0,
                                   NumModeratePeaks = 0,
                                   NumLowPeaks = 0,
                                   NumNonPeaks = total_days,
                                   MaxPeakValue = NA,
                                   FinalVerdict = NA
                                 ))
      ternary_data <- bind_rows(ternary_data,
                                data.frame(Crypto = crypto, Low = 0, Moderate = 0, High = 0, MaxPeakValue = NA))
    }
  }
  
  verdict_order <- c("High Peak", "Moderate Peak", "Low Peak")
  summary_table$FinalVerdict <- factor(summary_table$FinalVerdict, levels = verdict_order, ordered = TRUE)
  
  summary_table <- summary_table %>%
    arrange(desc(MaxPeakValue), FinalVerdict)
  
  list(summary_table = summary_table, ternary_data = ternary_data)
}

# ---------------------------
# 4. Export LaTeX / Excel
# ---------------------------
export_results <- function(summary_table, output_path, range_str, legend_str) {
  latex_file <- paste0(output_path, "peak_summary_", range_str, ".tex")
  sink(latex_file)
  print(xtable(summary_table), type = "latex", include.rownames = FALSE, include.colnames = TRUE)
  cat(paste0("\n\\vspace{2mm}\n\\footnotesize Legend: ", legend_str, ".\n"))
  sink()
  
  excel_file <- paste0(output_path, "peak_summary_", range_str, ".xlsx")
  summary_excel <- bind_rows(summary_table,
                             data.frame(Crypto = "Legend",
                                        NumHighPeaks = NA,
                                        NumModeratePeaks = NA,
                                        NumLowPeaks = NA,
                                        MaxPeakValue = NA,
                                        FinalVerdict = legend_str))
  write.xlsx(summary_excel, excel_file, rowNames = FALSE)
}

# ---------------------------
# 5. Ternary plot
# ---------------------------
plot_ternaryy <- function(ternary_data, range_str, output_path = NULL) {
  ternary_data$Dominant <- apply(ternary_data[,2:4], 1, function(x) c("Low","Moderate","High")[which.max(x)])
  
  actual_domains <- unique(ternary_data$Dominant)
  color_mapping <- c()
  if ("Low" %in% actual_domains) color_mapping["Low"] <- "green"
  if ("Moderate" %in% actual_domains) color_mapping["Moderate"] <- "orange"
  if ("High" %in% actual_domains) color_mapping["High"] <- "red"
  
  ternary_plot <- ggtern(data = ternary_data, 
                         aes(x = Moderate, y = High, z = Low, color = Dominant)) +
    geom_point(size = 3) +
    scale_color_manual(values = color_mapping) +
    labs(caption= paste("Peak Distribution by Using SVJ Model for Days", range_str),
         x = "Moderate", y = "High", z = "Low") +
    theme_bw() +
    theme(
      legend.position = c(0.25, 0.6),
      legend.margin   = margin(0, 0, 0, 0),
      legend.box      = "vertical",
      plot.caption    = element_text(size = 10, hjust = 0.5,
                                     margin = margin(t = -2), color = "black"),
      plot.margin     = margin(2, 2, 2, 2)   # << tighter margins
    )+
    geom_text(aes(label = Crypto), color = "black", size = 3, hjust = 0, vjust = 0)
  
  print(ternary_plot)
  
  if(!is.null(output_path)) {
    ggsave(filename = paste0(output_path, "SVJ_plot_", range_str, ".png"),
           plot = ternary_plot, width = 8, height = 6)
  }
  
  invisible(ternary_plot)
}

# ---------------------------
# 6. Combined stacked bar across all phases
# ---------------------------
plot_all_phases <- function(summary_table1, summary_table2, summary_table3,
                            phase1_label, phase2_label, phase3_label,
                            output_dir, file_name = "peaks_across_phases") {
  
  clean_table <- function(df, phase_label) {
    df %>%
      mutate(
        NumHighPeaks = ifelse(is.na(NumHighPeaks), 0, NumHighPeaks),
        NumModeratePeaks = ifelse(is.na(NumModeratePeaks), 0, NumModeratePeaks),
        NumLowPeaks = ifelse(is.na(NumLowPeaks), 0, NumLowPeaks),
        Phase = phase_label
      )
  }
  
  phase1 <- clean_table(summary_table1, phase1_label)
  phase2 <- clean_table(summary_table2, phase2_label)
  phase3 <- clean_table(summary_table3, phase3_label)
  
  all_data <- bind_rows(phase1, phase2, phase3)
  
  summary_data <- all_data %>%
    group_by(Phase) %>%
    summarise(
      High = sum(NumHighPeaks, na.rm = TRUE),
      Moderate = sum(NumModeratePeaks, na.rm = TRUE),
      Low = sum(NumLowPeaks, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = High:Low, names_to = "PeakType", values_to = "Count")
  
  totals <- summary_data %>%
    group_by(Phase) %>%
    summarise(Total = sum(Count))
  
  # Custom colors
  peak_colors <- c("High" = "red", "Moderate" = "orange", "Low" = "green")
  
  # Absolute stacked bar
  p1 <- ggplot(summary_data, aes(x = Phase, y = Count, fill = PeakType)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(data = totals, aes(x = Phase, y = Total, label = Total),
              inherit.aes = FALSE, vjust = -0.5, size = 5, fontface = "bold") +
    scale_fill_manual(values = peak_colors) +
    labs(title = "Distribution of Peak Types Across Phases (Absolute Counts)",
         x = "Phase", y = "Number of Peaks", fill = "Peak Type") +
    theme_minimal(base_size = 14)
  
  # Proportion stacked bar
  p2 <- ggplot(summary_data, aes(x = Phase, y = Count, fill = PeakType)) +
    geom_bar(stat = "identity", position = "fill", width = 0.5) +
    scale_fill_manual(values = peak_colors) +
    labs(title = "Distribution of Peak Types Across Phases (Proportions)",
         x = "Phase", y = "Proportion", fill = "Peak Type") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal(base_size = 14)
  
  print(p1)
  print(p2)
  
  ggsave(filename = file.path(output_dir, paste0(file_name, "_absolute_stacked_bar.png")), 
         plot = p1, width = 8, height = 6, dpi = 300)
  
  ggsave(filename = file.path(output_dir, paste0(file_name, "_proportion_stacked_bar.png")), 
         plot = p2, width = 8, height = 6, dpi = 300)
  
  return(list(absolute_plot = p1, proportion_plot = p2))
}

# ---------------------------
# 7. Main modular function
# ---------------------------
find_spikes_through_SVJ <- function(file_path, output_path, 
                              peak_quantiles = c(0.65,0.8,0.99),
                              row_ranges = c("1-30","30-60","60-100")) {
  
  df_prices_full <- load_crypto_data(file_path)
  legend_str <- paste0("Peak thresholds (quantiles): Low < ", peak_quantiles[1],
                       ", Moderate < ", peak_quantiles[2],
                       ", High >= ", peak_quantiles[3])
  
  all_summaries <- list()
  
  for(range_str in row_ranges) {
    bounds <- as.numeric(strsplit(range_str, "-")[[1]])
    df_prices <- df_prices_full[bounds[1]:bounds[2], , drop = FALSE]
    
    results <- summarize_peaks(df_prices, peak_quantiles)
    
    export_results(results$summary_table, output_path, range_str, legend_str)
    plot_ternaryy(results$ternary_data, range_str, output_path)
    
    tmp <- results$summary_table
    tmp$Phase <- range_str
    all_summaries[[range_str]] <- tmp
  }
  
  plot_all_phases(
    summary_table1 = all_summaries[[ row_ranges[1] ]],
    summary_table2 = all_summaries[[ row_ranges[2] ]],
    summary_table3 = all_summaries[[ row_ranges[3] ]],
    phase1_label = row_ranges[1],
    phase2_label = row_ranges[2],
    phase3_label = row_ranges[3],
    output_dir = output_path,
    file_name = "peaks_across_phases"
  )
  export_combined_jump_summary(all_summaries, output_path, legend_str)
}

# ---------------------------
# 8. Export Combined Summary of Jumps Across All Phases
# ---------------------------

# ---------------------------
# 8. Export Combined Summary of Jumps Across All Phases
# ---------------------------

export_combined_jump_summary <- function(all_summaries, output_path, legend_str) {
  
  # Initialize an empty dataframe to build our summary
  combined_summary <- data.frame(
    Phase = character(),
    High_Jumps = numeric(),
    Moderate_Jumps = numeric(),
    Low_Jumps = numeric(),
    Total_Jumps = numeric(), # New column added here
    stringsAsFactors = FALSE
  )
  
  # Loop through each phase in the all_summaries list
  for (phase_name in names(all_summaries)) {
    phase_data <- all_summaries[[phase_name]]
    
    # Calculate the total jumps for each category in this phase
    total_high <- sum(phase_data$NumHighPeaks, na.rm = TRUE)
    total_moderate <- sum(phase_data$NumModeratePeaks, na.rm = TRUE)
    total_low <- sum(phase_data$NumLowPeaks, na.rm = TRUE)
    total_all <- total_high + total_moderate + total_low # Calculate the total for the phase
    
    # Add a row for this phase to the combined summary (now with Total_Jumps)
    combined_summary <- rbind(combined_summary, data.frame(
      Phase = phase_name,
      High_Jumps = total_high,
      Moderate_Jumps = total_moderate,
      Low_Jumps = total_low,
      Total_Jumps = total_all
    ))
  }
  
  # Rename the phases for a cleaner look, e.g., "1-30" becomes "Day1-30"
  combined_summary$Phase <- paste0("Day", combined_summary$Phase)
  
  # ------- Generate LaTeX Output -------
  latex_file <- paste0(output_path, "combined_jumps_summary.tex")
  sink(latex_file)
  
  # Print the LaTeX table
  print(xtable(combined_summary), 
        type = "latex", 
        include.rownames = FALSE, 
        include.colnames = TRUE,
        sanitize.text.function = function(x){x})
  
  # Add the legend
  cat(paste0("\n\\vspace{2mm}\n\\footnotesize Legend: ", legend_str, ".\n"))
  sink()
  
  # ------- Generate Excel Output -------
  excel_file <- paste0(output_path, "combined_jumps_summary.xlsx")
  
  # Create a workbook
  wb <- createWorkbook()
  
  # Add a worksheet
  addWorksheet(wb, "Combined Jumps Summary")
  
  # Write the main data
  writeData(wb, sheet = 1, x = combined_summary, startRow = 1)
  
  # Write the legend in the row below the data
  legend_row <- nrow(combined_summary) + 2 # Leaving one empty row
  writeData(wb, sheet = 1, x = "Legend:", startRow = legend_row)
  writeData(wb, sheet = 1, x = legend_str, startRow = legend_row, startCol = 2)
  
  # Save the workbook
  saveWorkbook(wb, excel_file, overwrite = TRUE)
  
  message("Combined jump summary exported to:")
  message("  - ", latex_file)
  message("  - ", excel_file)
  
  return(combined_summary) # Return the summary in case you want to use it elsewhere
}