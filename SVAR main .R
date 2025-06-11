
install.packages("tidyquant")
install.packages("tidyverse")
install.packages("scales")
install.packages("ggplot")
install.packages("scales")
install.packages("data.table")
install.packages("timeSeries")
install.packages("varexternal")
install.packages("varexternalinstrument")
install.packages("ggplot2")
install.packages("svars")
install.packages("tseries")
install.packages("backtest")
install.packages("moments")
install.packages("readxl")
install.packages("writexl")
install.packages("openxlsx")
install.packages("zoo")
install.packages("xts")
install.packages("vars")
install.packages("AER")
install.packages("devtools")
install.packages("pracma")



library(pracma)
library(devtools)
library(openxlsx)
library(readxl)
library(writexl)
library(tidyverse)
library(tidyquant)
library(scales)
library(data.table)
library(highfrequency)
library(tseries)
library(moments)
library(readxl)
library(AER)
library(svars)
library(backtest)
library(stargazer)
library(xtable)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(boot)
library(varexternal)
library(varexternalinstrument)
library(zoo)
library(xts)
library(vars)



# Read Excel file into R
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


y1 = ts(c(NA,diff(log(COMP))), start=1997, frequency=12) #y1 = COMP return      Other Service Activities
y2 = ts(c(NA, diff(log(COD))), start=1997, frequency=12) #y2 = COD return.      Index of Services
y3 = ts(c(NA, diff(log(CST))), start=1997, frequency=12) #y3 = CST return.      Real estate activities
y4 = ts(c(NA, diff(log(ENE))), start=1997, frequency=12) #y4 = ENE return.      Distribution, Hotels and Restaurants
y5 = ts(c(NA, diff(log(FIN))), start=1997, frequency=12) #y5 = FIN return       Transport, Storage and Communications
y6 = ts(c(NA, diff(log(HCR))), start=1997, frequency=12) #y6 = HCR return.      Wholesale, retail and motor trade
y7 = ts(c(NA, diff(log(IND))), start=1997, frequency=12) #y7 = IND return.      Public administration and defence
y8 = ts(c(NA, diff(log(INT))), start=1997, frequency=12) #y8 = INT return.      Transportation and storage
y9 = ts(c(NA, diff(log(MAT))), start=1997, frequency=12) #y9 = MAT return.      Financial and insurance activities
y10 = ts(c(NA, diff(log(RES))), start=1997, frequency=12) #y10 = RES return.    Accommodation and food service activities
y11 = ts(c(NA, diff(log(TEL))), start=1997, frequency=12) #y11 = TEL return.    Education
y12 = ts(c(NA, diff(log(UTL))), start=1997, frequency=12) #y12 = UTL return.    Business Services and Finance
x1 = ts(c(NA, diff(log(RCI))), start=1997, frequency=12) #x1 = WTI return
iv1 = ts(c(NA, diff(log(HTO))), start=1997, frequency=12) #iv1 = HTO return
c1 = ts(c(NA, diff(log(IP))), start=1997, frequency=12) #c1 = IP growth rate
c2 = ts(c(NA, diff(log(IR))), start=1997, frequency=12) #c2 = IR monthly change
c3 = ts(c(NA, diff(log(WMKT))), start=1997, frequency=12) #c3 = Exchange rate GBP/USD
c4 = ts(c(NA, diff(log(TEN))), start=1997, frequency=12) #c4 = Unemployment rate
data_v = cbind(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, x1, iv1, c1, c2, c3, c4)

#First-Stage Regression

fs1 = lm(x1~c2+c1+iv1+c3+c4)
fs1res = c(NA,fs1$res)
fs1.fitted = ts(c(NA, fitted(fs1)), start = 1997, frequency = 12)


# Here, COMP is Other Service activities, which is Y1 is is col 13 in excel sheet 
#----
# Y1 (COMP) SVAR-IV
#-----------------------------------------------------------------------------------------------------------------------

y1.ivmat = window(ts.union(y1, fs1.fitted, c1, iv1, c3, c4), start = c(1997, 2), frequency = 12)

summary(y1.ivmat)


VARselect(y1.ivmat, lag.max=196, type="const")
var.y1ivsbc = VAR(y = y1.ivmat, p=1)
var.y1ivaic = VAR(y = y1.ivmat, p=3)

y1.ivc = id.chol(var.y1ivaic, order_k = c("y1", "fs1.fitted", "c1", "iv1", "c3", "c4"))
summary(y1.ivc)

irf_y1iv = irf(y1.ivc, n.ahead = 12, cumulative = FALSE, ci = 0.95)
#plot(irf_y1iv)
irf.y1_x1 = unlist(irf_y1iv$irf[3]) # Impulse Response of y1 to x1
#irf.x1_y1 = unlist(irf_y1iv$irf[8]) # Impulse Response of x1 to y1
bb_y1x1 = mb.boot(y1.ivc)
ba_y1x1 = ba.boot(bb_y1x1)
response.time = c(1:12)
irf.y1_x1_low = irf.y1_x1 - 1.96*sd(irf.y1_x1)
irf.y1_x1_high = irf.y1_x1 + 1.96*sd(irf.y1_x1)


plot(response.time, irf.y1_x1, xlim=c(2, 12), ylim=c(-0.005, 0.005), xlab = "", ylab = "percentage change", main = "Other Service Activities to credit spread shock", type = "l")
polygon(c(response.time, rev(response.time)), c(irf.y1_x1_low, rev(irf.y1_x1_high)), col='grey75', border=FALSE)
lines(response.time, irf.y1_x1, lwd=2)
lines(response.time, irf.y1_x1_low, col='red', lty=2)
lines(response.time, irf.y1_x1_high, col='red', lty=2)
#lines(response.time, irf.y1_x1_low, col=2)
#lines(response.time, irf.y1_x1_high, col=2)

fevd.y1iv = fevd(y1.ivc, n.ahead  = 24)

#----
# Y2 (COD) SVAR-IV
#-----------------------------------------------------------------------------------------------------------------------

y2.ivmat = window(ts.union(y2, fs1.fitted, c1, iv1, c3, c4), start = c(1997, 2), frequency = 12)

VARselect(y2.ivmat, lag.max=196, type="const")
var.y2ivsbc = VAR(y = y2.ivmat, p=1)
var.y2ivaic = VAR(y = y2.ivmat, p=3)

y2.ivc = id.chol(var.y2ivaic, order_k = c("y2", "fs1.fitted", "c1", "iv1", "c3", "c4"))
summary(y2.ivc)

irf_y2iv = irf(y2.ivc, n.ahead = 12, cumulative = FALSE, ci = 0.95)
#plot(irf_y2iv)
irf.y2_x1 = unlist(irf_y2iv$irf[3]) # Impulse Response of y2 to x1
response.time = c(1:12)
irf.y2_x1_low = irf.y2_x1 - 1.96*sd(irf.y2_x1)
irf.y2_x1_high = irf.y2_x1 + 1.96*sd(irf.y2_x1)
plot(response.time, irf.y2_x1, xlim=c(2, 12), ylim=c(-0.003, 0.004), xlab = "", ylab = "percentage change", main = "Index of Services to credit spread shock", type = "l")
polygon(c(response.time, rev(response.time)), c(irf.y2_x1_low, rev(irf.y2_x1_high)), col='grey75', border=FALSE)
lines(response.time, irf.y2_x1, lwd=2)
lines(response.time, irf.y2_x1_low, col='red', lty=2)
lines(response.time, irf.y2_x1_high, col='red', lty=2)

fevd.y2iv = fevd(y2.ivc, n.ahead  = 12)

#----
# Y3 (CST) SVAR-IV
#-----------------------------------------------------------------------------------------------------------------------

y3.ivmat = window(ts.union(y3, fs1.fitted, c1, iv1, c3, c4), start = c(1997, 2), frequency = 12)

VARselect(y3.ivmat, lag.max=196, type="const")
var.y3ivsbc = VAR(y = y3.ivmat, p=1)
var.y3ivaic = VAR(y = y3.ivmat, p=3)

y3.ivc = id.chol(var.y3ivaic, order_k = c("y3", "fs1.fitted", "c1", "c2", "c3", "c4"))
summary(y3.ivc)

irf_y3iv = irf(y3.ivc, n.ahead = 12, cumulative = FALSE, ci = 0.95)
#plot(irf_y3iv)
irf.y3_x1 = unlist(irf_y3iv$irf[3]) # Impulse Response of y3 to x1
response.time = c(1:12)
irf.y3_x1_low = irf.y3_x1 - 1.96*sd(irf.y3_x1)
irf.y3_x1_high = irf.y3_x1 + 1.96*sd(irf.y3_x1)
plot(response.time, irf.y3_x1, xlim=c(2, 12), ylim=c(-0.002, 0.002), xlab = "", ylab = "percentage change", main = "Real estate activities to credit spread Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(irf.y3_x1_low, rev(irf.y3_x1_high)), col='grey75', border=FALSE)
lines(response.time, irf.y3_x1, lwd=2)
lines(response.time, irf.y3_x1_low, col='red', lty=2)
lines(response.time, irf.y3_x1_high, col='red', lty=2)

fevd.y3iv = fevd(y3.ivc, n.ahead  = 12)

#----
# Y4 (ENE) SVAR-IV
#-----------------------------------------------------------------------------------------------------------------------

y4.ivmat = window(ts.union(y4, fs1.fitted, c1, iv1, c3, c4), start = c(1997, 2), frequency = 12)

VARselect(y4.ivmat, lag.max=196, type="const")
var.y4ivsbc = VAR(y = y4.ivmat, p=1)
var.y4ivaic = VAR(y = y4.ivmat, p=3)

y4.ivc = id.chol(var.y4ivaic, order_k = c("y4", "fs1.fitted", "c1", "iv1", "c3", "c4"))
summary(y4.ivc)

irf_y4iv = irf(y4.ivc, n.ahead = 12, cumulative = FALSE, ci = 0.95)
#plot(irf_y4iv)
irf.y4_x1 = unlist(irf_y4iv$irf[3]) # Impulse Response of y4 to x1
response.time = c(1:12)
irf.y4_x1_low = irf.y4_x1 - 1.96*sd(irf.y4_x1)
irf.y4_x1_high = irf.y4_x1 + 1.96*sd(irf.y4_x1)
plot(response.time, irf.y4_x1, xlim=c(2, 12), ylim=c(-0.004, 0.004), xlab = "", ylab = "Percentage change", main = "Distribution, Hotels and Restaurants to credit spread Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(irf.y4_x1_low, rev(irf.y4_x1_high)), col='grey75', border=FALSE)
lines(response.time, irf.y4_x1, lwd=2)
lines(response.time, irf.y4_x1_low, col='red', lty=2)
lines(response.time, irf.y4_x1_high, col='red', lty=2)

fevd.y4iv = fevd(y4.ivc, n.ahead  = 12)

#----
# Y5 (FIN) SVAR-IV
#-----------------------------------------------------------------------------------------------------------------------

y5.ivmat = window(ts.union(y5, fs1.fitted, c1, iv1, c3, c4), start = c(1997, 2), frequency = 12)

VARselect(y5.ivmat, lag.max=196, type="const")
var.y5ivsbc = VAR(y = y5.ivmat, p=1)
var.y5ivaic = VAR(y = y5.ivmat, p=3)

y5.ivc = id.chol(var.y5ivaic, order_k = c("y5", "fs1.fitted", "c1", "iv1","c3", "c4"))
summary(y5.ivc)

irf_y5iv = irf(y5.ivc, n.ahead = 12, cumulative = FALSE, ci = 0.95)
#plot(irf_y5iv)
irf.y5_x1 = unlist(irf_y5iv$irf[3]) # Impulse Response of y5 to x1
response.time = c(1:12)
irf.y5_x1_low = irf.y5_x1 - 1.96*sd(irf.y5_x1)
irf.y5_x1_high = irf.y5_x1 + 1.96*sd(irf.y5_x1)
plot(response.time, irf.y5_x1, xlim=c(2, 12), ylim=c(-0.004, 0.006), xlab = "", ylab = "percentage change", main = "Transport, Storage and Communications to credit spread shock", type = "l")
polygon(c(response.time, rev(response.time)), c(irf.y5_x1_low, rev(irf.y5_x1_high)), col='grey75', border=FALSE)
lines(response.time, irf.y5_x1, lwd=2)
lines(response.time, irf.y5_x1_low, col='red', lty=2)
lines(response.time, irf.y5_x1_high, col='red', lty=2)

fevd.y5iv = fevd(y5.ivc, n.ahead  = 12)

#----
# Y6 (HCR) SVAR-IV
#-----------------------------------------------------------------------------------------------------------------------

y6.ivmat = window(ts.union(y6, fs1.fitted, c1, iv1, c3, c4), start = c(1997, 2), frequency = 12)

VARselect(y6.ivmat, lag.max=12, type="const")
var.y6ivsbc = VAR(y = y6.ivmat, p=1)
var.y6ivaic = VAR(y = y6.ivmat, p=3)

y6.ivc = id.chol(var.y6ivaic, order_k = c("y6", "fs1.fitted", "c1", "iv1", "c3", "c4"))
summary(y6.ivc)

irf_y6iv = irf(y6.ivc, n.ahead = 12, cumulative = FALSE, ci = 0.95)
#plot(irf_y6iv)
irf.y6_x1 = unlist(irf_y6iv$irf[3]) # Impulse Response of y6 to x1
response.time = c(1:12)
irf.y6_x1_low = irf.y6_x1 - 1.96*sd(irf.y6_x1)
irf.y6_x1_high = irf.y6_x1 + 1.96*sd(irf.y6_x1)
plot(response.time, irf.y6_x1, xlim=c(2, 12), ylim=c(-0.004, 0.005), xlab = "", ylab = "Percentage change", main = "Wholesale, retail and motor tradeto credit spread shock", type = "l")
polygon(c(response.time, rev(response.time)), c(irf.y6_x1_low, rev(irf.y6_x1_high)), col='grey75', border=FALSE)
lines(response.time, irf.y6_x1, lwd=2)
lines(response.time, irf.y6_x1_low, col='red', lty=2)
lines(response.time, irf.y6_x1_high, col='red', lty=2)

fevd.y6iv = fevd(y6.ivc, n.ahead  = 24)

#----
# Y7 (IND) SVAR-IV
#-----------------------------------------------------------------------------------------------------------------------

y7.ivmat = window(ts.union(y7, fs1.fitted, c1, c2, c3, c4), start = c(1997, 2), frequency = 12)

VARselect(y7.ivmat, lag.max=24, type="const")
var.y7ivsbc = VAR(y = y7.ivmat, p=1)
var.y7ivaic = VAR(y = y7.ivmat, p=3)

y7.ivc = id.chol(var.y7ivaic, order_k = c("y7", "fs1.fitted", "c1", "c2", "c3", "c4"))
summary(y7.ivc)

irf_y7iv = irf(y7.ivc, n.ahead = 24, cumulative = FALSE, ci = 0.95)
#plot(irf_y7iv)
irf.y7_x1 = unlist(irf_y7iv$irf[3]) # Impulse Response of y7 to x1
response.time = c(1:24)
irf.y7_x1_low = irf.y7_x1 - 1.96*sd(irf.y7_x1)
irf.y7_x1_high = irf.y7_x1 + 1.96*sd(irf.y7_x1)
plot(response.time, irf.y7_x1, xlim=c(2, 24), ylim=c(-0.002, 0.0012), xlab = "", ylab = "percentage change", main = "Public administration and defence to credit spread Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(irf.y7_x1_low, rev(irf.y7_x1_high)), col='grey75', border=FALSE)
lines(response.time, irf.y7_x1, lwd=2)
lines(response.time, irf.y7_x1_low, col='red', lty=2)
lines(response.time, irf.y7_x1_high, col='red', lty=2)

fevd.y7iv = fevd(y7.ivc, n.ahead  = 24)

#----
# Y8 (INT) SVAR-IV
#-----------------------------------------------------------------------------------------------------------------------

y8.ivmat = window(ts.union(y8, fs1.fitted, c1, c2, c3, c4), start = c(1997, 2), frequency = 12)

VARselect(y8.ivmat, lag.max=196, type="const")
var.y8ivsbc = VAR(y = y8.ivmat, p=1)
var.y8ivaic = VAR(y = y8.ivmat, p=3)

y8.ivc = id.chol(var.y8ivaic, order_k = c("y8", "fs1.fitted", "c1", "c2", "c3", "c4"))
summary(y8.ivc)

irf_y8iv = irf(y8.ivc, n.ahead = 24, cumulative = FALSE, ci = 0.95)
#plot(irf_y8iv)
irf.y8_x1 = unlist(irf_y8iv$irf[3]) # Impulse Response of y8 to x1
response.time = c(1:24)
irf.y8_x1_low = irf.y8_x1 - 1.96*sd(irf.y8_x1)
irf.y8_x1_high = irf.y8_x1 + 1.96*sd(irf.y8_x1)
plot(response.time, irf.y8_x1, xlim=c(2, 24), ylim=c(-0.005, 0.005), xlab = "", ylab = "percentage change", main = "Transportation and storage to credit spread Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(irf.y8_x1_low, rev(irf.y8_x1_high)), col='grey75', border=FALSE)
lines(response.time, irf.y8_x1, lwd=2)
lines(response.time, irf.y8_x1_low, col='red', lty=2)
lines(response.time, irf.y8_x1_high, col='red', lty=2)

fevd.y8iv = fevd(y8.ivc, n.ahead  = 24)

#----
# Y9 (MAT) SVAR-IV
#-----------------------------------------------------------------------------------------------------------------------

y9.ivmat = window(ts.union(y9, fs1.fitted, c1, c2, c3, c4), start = c(1997, 2), frequency = 12)

VARselect(y9.ivmat, lag.max=196, type="const")
var.y9ivsbc = VAR(y = y9.ivmat, p=1)
var.y9ivaic = VAR(y = y9.ivmat, p=3)

y9.ivc = id.chol(var.y9ivaic, order_k = c("y9", "fs1.fitted", "c1", "c2", "c3", "c4"))
summary(y9.ivc)

irf_y9iv = irf(y9.ivc, n.ahead = 24, cumulative = FALSE, ci = 0.95)
#plot(irf_y9iv)
irf.y9_x1 = unlist(irf_y9iv$irf[3]) # Impulse Response of y9 to x1
response.time = c(1:24)
irf.y9_x1_low = irf.y9_x1 - 1.96*sd(irf.y9_x1)
irf.y9_x1_high = irf.y9_x1 + 1.96*sd(irf.y9_x1)
plot(response.time, irf.y9_x1, xlim=c(2, 24), ylim=c(-0.005, 0.005), xlab = "", ylab = "percentage change", main = "Financial and insurance to credit spread Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(irf.y9_x1_low, rev(irf.y9_x1_high)), col='grey75', border=FALSE)
lines(response.time, irf.y9_x1, lwd=2)
lines(response.time, irf.y9_x1_low, col='red', lty=2)
lines(response.time, irf.y9_x1_high, col='red', lty=2)

fevd.y9iv = fevd(y9.ivc, n.ahead  = 24)

#----
# Y10 (RES) SVAR-IV
#-----------------------------------------------------------------------------------------------------------------------

y10.ivmat = window(ts.union(y10, fs1.fitted, c1, c2, c3, c4), start = c(1997, 2), frequency = 12)

VARselect(y10.ivmat, lag.max=196, type="const")
var.y10ivsbc = VAR(y = y10.ivmat, p=1)
var.y10ivaic = VAR(y = y10.ivmat, p=3)

y10.ivc = id.chol(var.y10ivaic, order_k = c("y10", "fs1.fitted", "c1", "c2", "c3", "c4"))
summary(y10.ivc)

irf_y10iv = irf(y10.ivc, n.ahead = 24, cumulative = FALSE, ci = 0.95)
#plot(irf_y10iv)
irf.y10_x1 = unlist(irf_y10iv$irf[3]) # Impulse Response of y10 to x1
response.time = c(1:24)
irf.y10_x1_low = irf.y10_x1 - 1.96*sd(irf.y10_x1)
irf.y10_x1_high = irf.y10_x1 + 1.96*sd(irf.y10_x1)
plot(response.time, irf.y10_x1, xlim=c(2, 24), ylim=c(-0.005, 0.005), xlab = "", ylab = "percentage change", main = "Accommodation and food service activities to credit spread Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(irf.y10_x1_low, rev(irf.y10_x1_high)), col='grey75', border=FALSE)
lines(response.time, irf.y10_x1, lwd=2)
lines(response.time, irf.y10_x1_low, col='red', lty=2)
lines(response.time, irf.y10_x1_high, col='red', lty=2)

fevd.y10iv = fevd(y10.ivc, n.ahead  = 24)

#----
# Y11 (TEL) SVAR-IV
#-----------------------------------------------------------------------------------------------------------------------

y11.ivmat = window(ts.union(y11, fs1.fitted, c1, c2, c3, c4), start = c(1997, 2), frequency = 12)

VARselect(y11.ivmat, lag.max=196, type="const")
var.y11ivsbc = VAR(y = y11.ivmat, p=1)
var.y11ivaic = VAR(y = y11.ivmat, p=3)

y11.ivc = id.chol(var.y11ivaic, order_k = c("y11", "fs1.fitted", "c1", "c2", "c3", "c4"))
summary(y11.ivc)

irf_y11iv = irf(y11.ivc, n.ahead = 24, cumulative = FALSE, ci = 0.95)
#plot(irf_y11iv)
irf.y11_x1 = unlist(irf_y11iv$irf[3]) # Impulse Response of y11 to x1
response.time = c(1:24)
irf.y11_x1_low = irf.y11_x1 - 1.96*sd(irf.y11_x1)
irf.y11_x1_high = irf.y11_x1 + 1.96*sd(irf.y11_x1)
plot(response.time, irf.y11_x1, xlim=c(2, 24), ylim=c(-0.005, 0.005), xlab = "", ylab = "percenatge change", main = "Education to credit spread Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(irf.y11_x1_low, rev(irf.y11_x1_high)), col='grey75', border=FALSE)
lines(response.time, irf.y11_x1, lwd=2)
lines(response.time, irf.y11_x1_low, col='red', lty=2)
lines(response.time, irf.y11_x1_high, col='red', lty=2)

fevd.y11iv = fevd(y11.ivc, n.ahead  = 24)

#----
# Y12 (UTL) SVAR-IV
#-----------------------------------------------------------------------------------------------------------------------

y12.ivmat = window(ts.union(y12, fs1.fitted, c1, c2, c3, c4), start = c(1997, 2), frequency = 12)

VARselect(y12.ivmat, lag.max=196, type="const")
var.y12ivsbc = VAR(y = y12.ivmat, p=1)
var.y12ivaic = VAR(y = y12.ivmat, p=3)

y12.ivc = id.chol(var.y12ivaic, order_k = c("y12", "fs1.fitted", "c1", "c2", "c3", "c4"))
summary(y12.ivc)

irf_y12iv = irf(y12.ivc, n.ahead = 24, cumulative = FALSE, ci = 0.95)
#plot(irf_y12iv)
irf.y12_x1 = unlist(irf_y12iv$irf[3]) # Impulse Response of y12 to x1
response.time = c(1:24)
irf.y12_x1_low = irf.y12_x1 - 1.96*sd(irf.y12_x1)
irf.y12_x1_high = irf.y12_x1 + 1.96*sd(irf.y12_x1)
plot(response.time, irf.y12_x1, xlim=c(2, 24), ylim=c(-0.003, 0.0055), xlab = "", ylab = "percentage change", main = "Business Services and Finance to credit spread Shock", type = "l")
polygon(c(response.time, rev(response.time)), c(irf.y12_x1_low, rev(irf.y12_x1_high)), col='grey75', border=FALSE)
lines(response.time, irf.y12_x1, lwd=2)
lines(response.time, irf.y12_x1_low, col='red', lty=2)
lines(response.time, irf.y12_x1_high, col='red', lty=2)

fevd.y12iv = fevd(y12.ivc, n.ahead  = 24)

#----
# SVAR-IV test using new package from github
#-----------------------------------------------------------------------------------------------------------------------

p           = 3   #Number of lags in the VAR model
confidence  = .95    #Confidence Level for the standard and weak-IV robust confidence set
NWlags      = 0;  # Newey-West lags(if it is neccessary to account for time series autocorrelation)
norm        = 1; # Variable used for normalization
scale       = 1; # Scale of the shock
horizons    = 12; #Number of horizons for the Impulse Response Functions(IRFs)
instrument_name <- "iv1"  # Example name of the instrument
y1.data = window(ts.union(y1, x1, c1, c2, c3, c4), start = c(1997, 2), frequency = 12)
z.data = window(iv1, start = c(1997, 2), frequency = 12)



print(length(VAR$Plugin$IRF[1,]))
print(VAR$Plugin$IRF[1,])
print(length(VAR$Plugin$IRFstderror[1,]))
print(VAR$Plugin$IRFstderror[1,])

length(VAR$Plugin$IRF[1,])
length(VAR$Plugin$IRFstderror[1,])

print(VAR$Plugin$IRF[1,])
print(VAR$Plugin$IRFstderror[1,])

VAR = SVARIV(y1.data, z.data, p, confidence, NWlags, norm, scale, horizons, instrument_name ="iv1")

sh.col<-      c("#E41A1C")
names(sh.col)<-c("iv1")
pretty_irf(data=list(VAR$irfs),shock_names="iv1",pretty_names=c("a","b","c"),manual_color=sh.col,title="subheading")


dat<-melt(tibble(lag=seq(1:(horizons+1)),irf=VAR$Plugin$IRF[1,],lb=VAR$Plugin$IRF[1,]-1.96*VAR$Plugin$IRFstderror[1,],ub=VAR$Plugin$IRF[1,]+1.96*VAR$Plugin$IRFstderror[1,]),id="lag")

ggplot()+
  geom_line(data=subset(dat,variable=="irf"),aes(lag,value))+
  geom_line(data=subset(dat,variable=="ub"),aes(lag,value),colour="red")+
  geom_line(data=subset(dat,variable=="lb"),aes(lag,value),colour="red")+ 
  #geom_line(data=subset(d1,variable=="lb_old"),aes(lag,value))+ 
  #geom_line(data=subset(d1,variable=="ub_old"),aes(lag,value))+ 
  geom_hline(yintercept=0)+
  xlab("Time")+
  ylab("Response")+
  theme_bw()+
  theme(plot.margin = unit(c(5,5,5,5), "mm"))

print(length(VAR$Plugin$IRF[1,]))
print(VAR$Plugin$IRF[1,])
print(length(VAR$Plugin$IRFstderror[1,]))
print(VAR$Plugin$IRFstderror[1,])

# Ensure that the data lengths are as expected
irf <- VAR$Plugin$IRF[1,]
irf_stderr <- VAR$Plugin$IRFstderror[1,]

# Check that both vectors have the same length
if(length(irf) == length(irf_stderr) && length(irf) > 0) {
  lb <- irf - 1.96 * irf_stderr
  ub <- irf + 1.96 * irf_stderr
  
  dat <- tibble(
    lag = seq(1, horizons + 1),
    irf = irf,
    lb = lb,
    ub = ub
  ) %>% 
    melt(id = "lag")
  
  # Proceed with plotting
  ggplot() +
    geom_line(data = subset(dat, variable == "irf"), aes(lag, value)) +
    geom_line(data = subset(dat, variable == "ub"), aes(lag, value), colour = "red") +
    geom_line(data = subset(dat, variable == "lb"), aes(lag, value), colour = "red") +
    geom_hline(yintercept = 0) +
    xlab("Time") +
    ylab("Response") +
    theme_bw() +
    theme(plot.margin = unit(c(5, 5, 5, 5), "mm"))
} else {
  stop("The length of 'irf' and 'irf_stderr' are not the same or are zero. Check your input data.")
}




