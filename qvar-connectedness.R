

#Libraries that should be used

compute_qvar_connectedness <-function(file_path="/Users/sm25113/100days/last-dataset.xlsx"){

library(ConnectednessApproach)
library(frequencyConnectedness)
library(zoo)
library(kableExtra)
library(here)
library(readxl) ## you may also use CSV or txt data format for large scale data

setwd= here ()
 


# Import DATA
MODDATA = read_excel(file_path)

# Calculate log returns on the dataframe first
price_data <- MODDATA[, !grepl("Month", colnames(MODDATA))]
log_returns_data <- apply(price_data, 2, function(x) {
  returns <- diff(log(x))
  returns <- c(NA, returns)  # Add NA at beginning to maintain length
  return(returns)
})

# Remove first row with NA values
log_returns_data <- log_returns_data[-1, ]
MODDATA <- MODDATA[-1, ]  # Also remove first row from original data to match

# Use ZOO library with log returns data
MD = zoo(x = log_returns_data, order.by = MODDATA$Month)

# INITIALISATION
nlag = 1
tau = 0.5
nfore = 20
corrected = TRUE
window.size = 24
kableExtra::kable(SummaryStatistics(MD))

## The following statistics are used:
## 
##           Skewness: D'Agostino, R.B. (1970). Transformation to Normality of the Null Distribution of G1. Biometrika, 57, 3, 679-681.
## 
##           Excess Kurtosis: Anscombe, F.J., Glynn, W.J. (1983) Distribution of kurtosis statistic for normal statistics. Biometrika, 70, 1, 227-234
## 
##           Normality test: Jarque, C. M., & Bera, A. K. (1980). Efficient tests for normality, homoscedasticity and serial independence of regression residuals. Economics Letters, 6(3), 255-259.
## 
##           ERS unit-root test: Elliott, G., Rothenberg, T. J., & Stock, J. H. (1996). Efficient Tests for an Autoregressive Unit Root. Econometrica, 64(4), 813-836.
## 
##           Weighted Portmanteau statistics: Fisher, T. J., & Gallagher, C. M. (2012). New weighted portmanteau statistics for time series goodness of fit testing. Journal of the American Statistical Association, 107(498), 777-787.

#### DCA Calculation Formula ###
dca = ConnectednessApproach(MD,  # Now MD contains log returns
                            model="QVAR",
                            connectedness="Time",
                            nlag=nlag,
                            nfore=nfore,
                            corrected=corrected,
                            window.size=window.size,
                            VAR_config=list(QVAR=list(tau=tau)))

## For Connectedness Table
kableExtra::kable(dca$TABLE)

##Plot TCI DCA

PlotTCI(dca)

##Plot NET DCA

PlotNET(dca)

## Plotting Quantile Heatmaps
## Step 1:

k = ncol(MD)
t = nrow(dca$TCI)
date = index(MD)
NAMES = colnames(MD)

n = 10
quantiles = seq(0.05, 0.95, 1/n) 
print(quantiles)

## Step 2:

DCA_list = list()
TCI = array(NA, c(t, length(quantiles)), dimnames=list(as.character(tail(date,t)), quantiles))
NET = array(NA, c(t, k, length(quantiles)), dimnames=list(as.character(tail(date,t)), NAMES, quantiles))
for (j in 1:length(quantiles)) {
  dca = suppressMessages(ConnectednessApproach(MD,
                                               model="QVAR",
                                               connectedness="Time",
                                               nlag=nlag,
                                               nfore=nfore,
                                               corrected=corrected,
                                               window.size=window.size,
                                               VAR_config=list(QVAR=list(tau=quantiles[j]))))
  TCI[,j] = dca$TCI
  NET[,,j] = dca$NET
  DCA_list = c(DCA_list, list(dca$TABLE))
  print(quantiles[j])
}

## Step 3: It will give you the dynamic total connectedness heatmap (IT WILL TAKE TIME for CALCULATION: Just Wait)

nlevels = 20
threshold = 0.01 # get rid of outliers
filled.contour(tail(date,t), quantiles, TCI, xlab="", ylab="", ylim=c(0,1))

## Step 4: It will gives you the heatmap related to NET directional connectedness for each variable in the

for (i in 1:k) {
  net = NET[,i,,drop=FALSE]
  net[which(net<quantile(net, threshold), arr.ind=TRUE)] = as.numeric(quantile(net, threshold))
  net[which(net>quantile(net, 1-threshold), arr.ind=TRUE)] = as.numeric(quantile(net, 1-threshold))
  
  lvls = seq(-ceiling(max(abs(net))), ceiling(max(abs(net))), length.out=nlevels)
  color.palette = function(n) {hcl.colors(n, "RdBu", rev=TRUE)}
  quantiles = as.numeric(dimnames(net)[[3]])
  filled.contour(tail(date,t), quantiles, as.matrix(net[,1,]),
                 col=color.palette(nlevels-1), levels=lvls, main=colnames(net), ylim=c(0,1))
}

## Step 5: To generate the NetworkPlot (Here for 0.5 quantiles as we put it in the initialization)

PlotNetwork(dca, method="NPDC")
mtext("Quantile = 0.50", 
      side = 2,    # Left side (1=bottom, 2=left, 3=top, 4=right)
      line = 3,    # Adjust distance from the plot edge
      adj = 0.5,   # Centered vertically (0=bottom, 1=top)
      cex = 1.2,   # Text size
      font = 2,    # Bold text
      col = "black")



## Now if you wants to assess the networkplot for particular quantiles then you have to change the quantiles at the time of initialization

########## Network Plot ###########

#tau= 0.05


# INITIALISATION

nlag = 1
tau = 0.3
nfore = 20
corrected = TRUE
window.size = 24


#### DCA Calculation Formula ###
dca = ConnectednessApproach(MD,
                            model="QVAR",
                            connectedness="Time",
                            nlag=nlag,
                            nfore=nfore,
                            corrected=corrected,
                            window.size=window.size,
                            VAR_config=list(QVAR=list(tau=tau)))
##To generate the NetworkPlot
PlotNetwork(dca, method="NPDC")
mtext("Quantile = 0.05", 
      side = 2,    # Left side (1=bottom, 2=left, 3=top, 4=right)
      line = 3,    # Adjust distance from the plot edge
      adj = 0.5,   # Centered vertically (0=bottom, 1=top)
      cex = 1.2,   # Text size
      font = 2,    # Bold text
      col = "black")



#tau= 0.25


# INITIALISATION

nlag = 1
tau = 0.6
nfore = 20
corrected = TRUE
window.size = 24

#### DCA Calculation Formula ###
dca = ConnectednessApproach(MD,
                            model="QVAR",
                            connectedness="Time",
                            nlag=nlag,
                            nfore=nfore,
                            corrected=corrected,
                            window.size=window.size,
                            VAR_config=list(QVAR=list(tau=tau)))
##To generate the NetworkPlot
PlotNetwork(dca, method="NPDC")
mtext("Quantile = 0.25", 
      side = 2,    # Left side (1=bottom, 2=left, 3=top, 4=right)
      line = 3,    # Adjust distance from the plot edge
      adj = 0.5,   # Centered vertically (0=bottom, 1=top)
      cex = 1.2,   # Text size
      font = 2,    # Bold text
      col = "black")

#tau= 0.75


# INITIALISATION

nlag = 1
tau = 0.9
nfore = 20
corrected = TRUE
window.size = 24

#### DCA Calculation Formula ###
dca = ConnectednessApproach(MD,
                            model="QVAR",
                            connectedness="Time",
                            nlag=nlag,
                            nfore=nfore,
                            corrected=corrected,
                            window.size=window.size,
                            VAR_config=list(QVAR=list(tau=tau)))
##To generate the NetworkPlot
PlotNetwork(dca, method="NPDC")
mtext("Quantile = 0.75", 
      side = 2,    # Left side (1=bottom, 2=left, 3=top, 4=right)
      line = 3,    # Adjust distance from the plot edge
      adj = 0.5,   # Centered vertically (0=bottom, 1=top)
      cex = 1.2,   # Text size
      font = 2,    # Bold text
      col = "black")


#tau= 0.95


# INITIALISATION

nlag = 1
tau = 0.95
nfore = 20
corrected = TRUE
window.size = 24

#### DCA Calculation Formula ###
dca = ConnectednessApproach(MD,
                            model="QVAR",
                            connectedness="Time",
                            nlag=nlag,
                            nfore=nfore,
                            corrected=corrected,
                            window.size=window.size,
                            VAR_config=list(QVAR=list(tau=tau)))
##To generate the NetworkPlot
PlotNetwork(dca, method="NPDC")
# 1. Generate the network plot
mtext("Quantile = 0.95", 
      side = 2,    # Left side (1=bottom, 2=left, 3=top, 4=right)
      line = 3,    # Adjust distance from the plot edge
      adj = 0.5,   # Centered vertically (0=bottom, 1=top)
      cex = 1.2,   # Text size
      font = 2,    # Bold text
      col = "black")
}
