# ----------------------------------------------
# Required Packages
# ----------------------------------------------
install.packages("writexl")
install.packages("devtools")
devtools::install_github("GabauerDavid/ConnectednessApproach")
install.packages("kableExtra")
install.packages("moments")
install.packages("tseries")
install.packages("e1071")
install.packages("urca")

# Install Packages
install.packages("ConnectednessApproach")
install.packages("frequencyConnectedness")
install.packages("zoo")
install.packages("kableExtra")
install.packages("here")
install.packages("readxl")
install.packages(c("ggtern", "gridExtra", "gtable", "openxlsx", "xtable", "MSwM", "readxl", "dplyr"))


install.packages("fmsb")



# ----------------------------------------------
# Load Libraries
# ----------------------------------------------


source("library.R")


file_path <- "/Users/sm25113/100days/dataset.xlsx"
output_path <- "/Users/sm25113/100days/output/"


source("descriptive-stat.R")
generate_complete_descriptive_tables(file_path,output_path)

source("market_states.R")
classify_market_states(file_path, output_path, row_ranges = c("1-30", "30-60", "60-100"))

source("tt-test.R")
generate_phase_level_summary("/Users/sm25113/100days/output/phase_wise_stats.xlsx", output_path)




source("Morkov-Switching-States.R")
Markove_Switching_Model_trends(file_path,output_path,row_ranges = c("1-30","30-60","60-100"))

source("Markov-Switching-Volatality.R")
find_volatality_using_markov_switching(file_path,output_path,row_ranges = c("1-30","30-60","60-100"))


source("Stochastic-Volatility-Jumps.R", echo = TRUE)
find_spikes_through_SVJ(file_path,output_path,row_ranges = c("1-30","30-60","60-100"))



# ----------------------------------------------
# END OF SCRIPT
# ----------------------------------------------

source("qvar-connectedness.R")
compute_qvar_connectedness("/Users/sm25113/100days/last-dataset.xlsx")











final_volatility_verdict_robust(file_path)

final_volatility_verdict_raw(file_path)

final_volatility_verdict(file_path)

compute_volatility_table(file_path)

generate_summary_table(file_path)



source("library.R")





# Example usage:
# Provide the correct file path to your Excel file
file_path <- "path/to/your/crypto_data.xlsx"  # Replace with actual file path
result <- perform_crypto_ttest(file_path)

# Print the result
print(result)


print(data)


plot_crypto_prices(file_path, "100-days",rows = 1:2, cols = 15:20)

plot_crypto_logreturns(file_path, "100-days",rows = 2:4, cols = 1:5)


save_log_returns(file_path)




generate_descriptive_stat(data, output_path)

# ----------------------------------------------
# Compute Returns
# ----------------------------------------------
data_return <- getReturns(file_path)
print(data_return)

# ----------------------------------------------
# Question 1 - Crypto Clusters
# ----------------------------------------------
all_columns <- c(2,3,4)
networkAnalysis(data_return, all_columns, output_path, "pi-trump-doge")

all_columns <- c(5,6,7)
networkAnalysis(data_return, all_columns, output_path, "btc-eth-xrp")

all_columns <- c(2,3,4,5,6,7)
networkAnalysis(data_return, all_columns, output_path, "pi-trump-doge-btc-eth-xrp")

# ----------------------------------------------
# Question 2 - Crypto + Regions
# ----------------------------------------------
all_columns <- c(2,3,4,5,6,7,8,15)
networkAnalysis(data_return, all_columns, output_path, "pi-trump-doge-btc-eth-xrp-region-EAP")

all_columns <- c(2,3,4,5,6,7,9,16)
networkAnalysis(data_return, all_columns, output_path, "pi-trump-doge-btc-eth-xrp-region-EU")

all_columns <- c(2,3,4,5,6,7,10,17)
networkAnalysis(data_return, all_columns, output_path, "pi-trump-doge-btc-eth-xrp-region-LAC")

all_columns <- c(2,3,4,5,6,7,11,18)
networkAnalysis(data_return, all_columns, output_path, "pi-trump-doge-btc-eth-xrp-region-MENA")

all_columns <- c(2,3,4,5,6,7,12,19)
networkAnalysis(data_return, all_columns, output_path, "pi-trump-doge-btc-eth-xrp-region-NA")

all_columns <- c(2,3,4,5,6,7,13,20)
networkAnalysis(data_return, all_columns, output_path, "pi-trump-doge-btc-eth-xrp-region-SA")

all_columns <- c(2,3,4,5,6,7,14,21)
networkAnalysis(data_return, all_columns, output_path, "pi-trump-doge-btc-eth-xrp-region-SSA")

# ----------------------------------------------
# Question 3 - Crypto + Sin/Social
# ----------------------------------------------
all_columns <- c(2,3,4,8:14)
networkAnalysis(data_return, all_columns, output_path, "pi-trump-doge-sin")

all_columns <- c(2,3,4,15:21)
networkAnalysis(data_return, all_columns, output_path, "pi-trump-doge-social")

all_columns <- c(5,6,7,8:14)
networkAnalysis(data_return, all_columns, output_path, "btc-eth-xrp-sin")

all_columns <- c(5,6,7,15:21)
networkAnalysis(data_return, all_columns, output_path, "btc-eth-xrp-social")

all_columns <- c(2:21)
networkAnalysis(data_return, all_columns, output_path, "pi-trump-doge-btc-eth-xrp-sin-social")





