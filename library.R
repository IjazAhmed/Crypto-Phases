clean_Rstudio <- function() {
  # Clear objects from environment
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  
  # Clear plots (close all graphic devices)
  while (!is.null(dev.list())) dev.off()
  
  # Clear console (works only inside RStudio)
  if (rstudioapi::isAvailable()) {
    rstudioapi::sendToConsole("cat('\\014')", execute = TRUE)
  } else {
    cat("\014")  # fallback
  }
  
  # Clear history
  try(file.remove(".Rhistory"), silent = TRUE)
  
  message("✅ RStudio environment cleaned!")
}

clean_Rstudio()


# Data I/O
library(readxl)
library(openxlsx)


library(writexl)

# Data manipulation
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)

# Time series & statistics
library(ConnectednessApproach)
library(MSwM)
library(urca)
library(tseries)
library(moments)
library(e1071)
library(pracma)

# Reporting & tables
library(knitr)
library(xtable)
library(kableExtra)

# Visualization
library(plotly)
library(ggtern)
library(ggrepel)
library(RColorBrewer)

library(fmsb)
library(dplyr)