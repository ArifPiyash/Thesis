install.packages("tidyquant")
install.packages("tidyverse")
install.packages("scales")
install.packages("ggplot")
install.packages("scales")
install.packages("data.table")
install.packages("ggplot2")
install.packages("moments")
install.packages("readxl")
install.packages("writexl")
install.packages("openxlsx")
install.packages("xts")
install.packages("AER")
install.packages("devtools")
install.packages("pracma")
install.packages("ivDiag")
install.packages("car")
install.packages("carData")
install.packages("estimatr")



library(pracma)
library(devtools)
library(openxlsx)
library(readxl)
library(writexl)
library(tidyverse)
library(tidyquant)
library(scales)
library(data.table)
library(moments)
library(readxl)
library(AER)
library(stargazer)
library(xtable)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(boot)
library(xts)
library(ivDiag)
library(car)
library(carData)
library(estimatr)

data = read_excel("Documents/SVAR-IV-main/data_test.xlsx")

COD = ts(data[,2], start = 1997, frequency = 12) #Index of Services
ENE = ts(data[,3], start = 1997, frequency = 12) # Distribution, Hotels and Restaurants
FIN = ts(data[,4], start = 1997, frequency = 12) #Transport, Storage and Communications
UTL = ts(data[,5], start = 1997, frequency = 12) #Business Services and Finance
HCR = ts(data[,6], start = 1997, frequency = 12) #Wholesale, retail and motor trade
INT = ts(data[,7], start = 1997, frequency = 12) #Transportation and storage
RES = ts(data[,8], start = 1997, frequency = 12) #Accommodation and food service activities 
IND = ts(data[,9], start = 1997, frequency = 12) #Public administration and defence 
MAT = ts(data[,10], start = 1997, frequency = 12) #Financial and insurance activities 
CST = ts(data[,11], start = 1997, frequency = 12) #Real estate activities
TEL = ts(data[,12], start = 1997, frequency = 12) #Education
COMP = ts(data[,13], start = 1997, frequency = 12) #Other Service Activities
RCI = ts(data[,14], start = 1997, frequency = 12) #Real credit index
HTO = ts(data[,15], start = 1997, frequency = 12) #LIBOR Rate monthly
IP = ts(data[,16], start = 1997, frequency = 12) #GDP UK
IR = ts(data[,17], start = 1997, frequency = 12) #CPI
WMKT = ts(data[,18], start = 1997, frequency = 12) #Exchange rate GBP/USD
TEN = ts(data[,19], start = 1997, frequency = 12) #Unemployment rate

y1 = ts(c(NA,diff(log(COMP))), start=1997, frequency=12) #y1 = COMP return
y2 = ts(c(NA, diff(log(COD))), start=1997, frequency=12) #y2 = COD return
y3 = ts(c(NA, diff(log(CST))), start=1997, frequency=12) #y3 = CST return
y4 = ts(c(NA, diff(log(ENE))), start=1997, frequency=12) #y4 = ENE return
y5 = ts(c(NA, diff(log(FIN))), start=1997, frequency=12) #y5 = FIN return
y6 = ts(c(NA, diff(log(HCR))), start=1997, frequency=12) #y6 = HCR return
y7 = ts(c(NA, diff(log(IND))), start=1997, frequency=12) #y7 = IND return
y8 = ts(c(NA, diff(log(INT))), start=1997, frequency=12) #y8 = INT return
y9 = ts(c(NA, diff(log(MAT))), start=1997, frequency=12) #y9 = MAT return
y10 = ts(c(NA, diff(log(RES))), start=1997, frequency=12) #y10 = RES return
y11 = ts(c(NA, diff(log(TEL))), start=1997, frequency=12) #y11 = TEL return
y12 = ts(c(NA, diff(log(UTL))), start=1997, frequency=12) #y12 = UTL return
x1 = ts(c(NA, diff(log(RCI))), start=1997, frequency=12) #x1 = WTI return
iv1 = ts(c(NA, diff(log(HTO))), start=1997, frequency=12) #iv1 = HTO return
c1 = ts(c(NA, diff(log(IP))), start=1997, frequency=12) #c1 =  GDP UK
c2 = ts(c(NA, diff(IR)), start=1997, frequency=12) #c2 = CPI 
c3 = ts(c(NA, diff(log(WMKT))), start=1997, frequency=12) #c3 = Exchange rate GBP/USD
c4 = ts(c(NA, diff(log(TEN))), start=1997, frequency=12) #c4 = Unemployment rate



# Variables
Y <- "y2" # Y: outcome of interest
D <- "x1" # D: endogenous treatment
Z <- "c2" # Z: instrumental variable
controls <- c("c1", "iv1", "c3", "c4") # covariates of control variables
cl <- NULL # No clustering variable available in this context
weights <- FE <- NULL # no weights or fixed effects


# Plotting using the time series objects directly
par(mar = c(4, 4, 2, 2))
plot(c2, x1, col = "#777777", cex = 0.5, 
     main = "Raw Data", xlab = "Instrument (CPI)", ylab = "Treatment (Real Credit Index)")
abline(lm(x1 ~ c2), col = 2, lwd = 2, lty = 2)



# Combine all relevant time series data into a data frame
df <- data.frame(
  y2 = as.numeric(y2),
  x1 = as.numeric(x1),
  c2 = as.numeric(c2),
  c1 = as.numeric(c1),
  iv1 = as.numeric(iv1),
  c3 = as.numeric(c3),
  c4 = as.numeric(c4)
)

# Remove any rows with NA values (resulting from diff/log transformations)
df <- na.omit(df)

# Define your variables
Y <- "y2"  # Outcome of interest
D <- "x1"  # Endogenous treatment
Z <- "c2"  # Instrumental variable
controls <- c("c1", "iv1", "c3", "c4")  # Control variables
cl <- NULL  # No clustering in this example

# Run the ivDiag function
g <- ivDiag(data = df, Y = Y, D = D, Z = Z, controls = controls, cl = cl)

# Explore the output
names(g)
 
# OLS estimation results
summary(g$est_ols)

# 2SLS estimation results
summary(g$est_2sls)
  

# Conduct the effective F statistic test
F_statistic <- eff_F(data = df, Y = "y2", D = "x1", Z = "c2", controls = c("c1", "iv1", "c3", "c4"), cl = NULL, FE = NULL, weights = NULL)

# Print the result
summary(F_statistic)

# Conduct the AR test
AR_results <- AR_test(data = df, Y = "y2", D = "x1", Z = "c2", controls = c("c1", "iv1", "c3", "c4"), cl = NULL, FE = NULL, weights = NULL)

# Print the results
summary(AR_results)

stargazer(g$est_2sls, type = "text",
          add.lines = list(c("F-statistic", "11.424"),
                           c("AR Confidence Interval", "[-0.6550, 1.8326]")),
          title = "Regression Results OLS",
          dep.var.labels = c("OLS"),
          covariate.labels = c("Coefficient", "Standard Error", "t-statistic", "CI 2.5%", "CI 97.5%", "p-value"),
          column.labels = c("OLS"),
          align = TRUE)

stargazer(g$est_ols, type = "text",
          add.lines = list(c("F-statistic", "11.424"),
                           c("AR Confidence Interval", "[-0.6550, 1.8326]")),
          title = "Regression Results OLS",
          dep.var.labels = c("OLS"),
          covariate.labels = c("Coefficient", "Standard Error", "t-statistic", "CI 2.5%", "CI 97.5%", "p-value"),
          column.labels = c("OLS"),
          align = TRUE)



stargazer(g$est_ols, g$est_2sls, F_statistic, AR_results)





# Conduct the ZFS test using a robust linear model
zfs <- lm_robust(y2 ~ c2 + c1 + iv1 + c3 + c4, data = df, 
                 weights = NULL, se_type = "HC1")

# Extract the coefficient and standard error for the instrument
zfs_coef <- summary(zfs)$coefficients["c2", 1:2]
zfs_coef

# Conduct the LTZ adjustment
ltz_out <- ltz(data = df, Y = "y2", D = "x1", Z = "c2", 
               controls = c("c1", "iv1", "c3", "c4"), weights = NULL, 
               prior = c(zfs_coef[1], zfs_coef[2]))

# Display the LTZ adjustment results
ltz_out

# Plot the LTZ adjustment
plot_ltz(ltz_out, xlim = c(-0.5, 2))

# Alternatively, use the components separately
plot_ltz(iv_est = ltz_out$iv[1:2], ltz_est = ltz_out$ltz[1:2], 
         prior = ltz_out$prior)
# Plot the LTZ adjustment with adjusted x-axis limits
plot_ltz(ltz_out, xlim = c(-2, 2))  # Adjust xlim to better suit the scale

# Alternatively, plot the estimates separately with customized xlim
plot_ltz(
  iv_est = ltz_out$iv[1:2], 
  ltz_est = ltz_out$ltz[1:2], 
  prior = ltz_out$prior, 
  xlim = c(-2, 2)
)

# Adjust the prior to a more plausible range for better visualization
adjusted_prior <- c(-0.1, 0.1)  # Example: Broaden the prior range

# Re-run the LTZ adjustment with the adjusted prior
ltz_out_adjusted <- ltz(
  data = df, Y = "y2", D = "x1", Z = "c2", 
  controls = c("c1", "iv1", "c3", "c4"), weights = NULL, 
  prior = adjusted_prior
)

# Plot with the adjusted prior
plot_ltz(ltz_out_adjusted, xlim = c(-2, 2))
g$F_stat


  







