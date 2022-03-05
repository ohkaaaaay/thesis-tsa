### Install & Load Packages #####
#install.packages(c("zoo", "forecast", "tseries", "lmtest", "TSA", "tsoutliers"))
library(zoo)
library(forecast)
library(tseries)
library(TSA)
library(lmtest)
library(tsoutliers)
library(ggplot2)
library(dplyr)

##### Read CSV File #####
res.yearly <- read.csv("lawful_res.csv")
res <- res.yearly$Number
fy <- res.yearly$Year

##### Plotting Time Series #####
## Time series plot
# Regular plot
par(mfrow=c(1,1))
plot(fy, res, type="l", main="Annual Lawful Permanent Residents (FY 1820 - 2020)",
     xlab="Year", ylab="Number")
# ggplot2
par(mfrow=c(1,1))
ggplot(data=res.yearly, aes(x=Year, y=Number)) + geom_line() +
labs(title="Annual Lawful Permanent Residents (FY 1820 - 2020)",
     x="Year", y="Number")

## Intervention events annotated
# Regular plot
par(mfrow=c(1,1))
plot(fy, res, type="o", main="Annual Lawful Permanent Residents (FY 1820 - 2020)",
     xlab="Year", ylab="Number")
# WWI
abline(v=1915, lty=2, col='black')
abline(v=1919, lty=2, col='black')
# Great Depression
abline(v=1931, lty=2, col='red')
abline(v=1938, lty=2, col='red')
# WWII
abline(v=1940, lty=2, col='blue')
abline(v=1945, lty=2, col='blue')
# IRCA
abline(v=1989, lty=2, col='green')
abline(v=1991, lty=2, col='green')
# Legend
legend("topleft", c("World War I", "Great Depression", "World War II", "IRCA"),
       col=c("black", "red", "blue", "green"), lty=c(2,2))

# ggplot2
par(mfrow=c(1,1))
ggplot(data=res.yearly, aes(x=Year, y=Number)) + geom_line() +
  geom_point(alpha=0.5) +
  # World War I
  geom_vline(aes(xintercept=1915, color="World War I"), linetype="dashed") +
  geom_vline(aes(xintercept=1918, color="World War I"), linetype="dashed") +
  # Great Depression
  geom_vline(aes(xintercept=1931, color="Great Depression"), linetype="dashed") +
  geom_vline(aes(xintercept=1938, color="Great Depression"), linetype="dashed") +
  # World War II
  geom_vline(aes(xintercept=1940, color="World War II"), linetype="dashed") +
  geom_vline(aes(xintercept=1945, color="World War II"), linetype="dashed") +
  # IRCA
  geom_vline(aes(xintercept=1989, color="IRCA"), linetype="dashed") +
  geom_vline(aes(xintercept=1991, color="IRCA"), linetype="dashed") +
  # Labels
  labs(title="Annual Lawful Permanent Residents (FY 1820 - 2020)",
       x="Year", y="Number", color="Intervention Events")

# Histogram
par(mfrow=c(1,1))
hist(res, main="Annual LPR Distribution", xlab="LPR")
# ggplot2
par(mfrow=c(1,1))
ggplot(res.yearly, aes(x=res)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=20) +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Annual LPR Distribution", x="LPR", y="Frequency")

# Descriptive statistics
# Standard (mean, std dev, max, min)
summary(res)
sd(res)

# Check stationarity
par(mfrow=c(1,2))
acf(res)
pacf(res)

# Check for constant variance
# Load library for Box-Cox lambda
# Check lambda value
# If lambda=0, a log transformation is needed
BoxCox.lambda(res) # -0.154 - Apply log
# Log transformation applied
log.res <- log(res)

# Check for constant mean
# Augmented Dickey-Fuller unit root test
adf.test(log.res) # Data is not stationary
# Difference log data
dlog.res <- diff(log.res)
adf.test(dlog.res) # Data is stationary

# Plot stationary dataset
par(mfrow=c(1,1))
plot(fy[2:201], dlog.res, type="l", main="Log & Difference of Lawful Permanent Residence",
     xlab="Year", ylab="Number")

##### Determine Model Order #####
# EACF
eacf(dlog.res)

# ACF & PACF
par(mfrow=c(1,2))
acf(dlog.res) # MA(12)
pacf(dlog.res) # AR(5)

# Model Build
# MA(12)
out1 <- arima(dlog.res, order=c(0, 0, 12))
out1$aic # Model not selected
# AR(5)
out2 <- arima(dlog.res, order=c(5, 0, 0))
out2$aic # Model selected

# Apply difference in model build
# AR(5)
out2.1 <- arima(log.res, order = c(5, 1, 0))
out2.1$aic # Model selected

##### Remove Non-Significant Coefficients #####
coeftest(out2.1)
out2.2 <- arima(log.res, order=c(5, 1, 0), fixed=c(0,0,0,0,NA))
out2.2$aic # Model selected

##### Model Diagnostics #####
# 1) Residual Analysis
# Plot residuals
par(mfrow=c(1,1))
plot(out2.2$residuals, type='l', ylim=c(-1.5,1.5),
     main="Residual Analysis of Lawful Permanent Residence")
sde <- sqrt(out2.2$sigma2) # Standard deviation of error
cfi <- 2*sde # Confidence interval
abline(h=cfi, lty=2) # Upper confidence interval
abline(h=-cfi, lty=2) # Lower confidence interval

# Check for outliers
locate.outliers(out2.2$residuals, par=coefs2poly(out2.2))

# ACF and PACF
par(mfrow = c(1,2))
pacf(out2.2$residuals)
acf(out2.2$residuals)
# Residuals appears to be white noise
# Need to make final determination using Box-Ljung test

# Box-Ljung test
Box.test(out2.2$residuals, lag=12, type="Ljung")
# Residuals are white noise

# 2) Check for Overspecification
# Go to "Remove Non-Significant Coefficients" section

# 3) Check for Stationary through AR Roots
polyroot(c(1, -out2.2$coef[5]))
abs(polyroot(c(1, -out2.2$coef[5])))
# Root not close to 1

# 4) Check for Model Redundancy through AR and MA Roots
# No MA roots

##### Intervention Events #####
source('FUNCTIONS_Intervention_Events.R')
# Prepare variables
x <- ts(res, start=1820, frequency=1)
n <- length(x)
order <- c(5,1,0)
fixed <- c(0,0,0,0,NA)

# 1915-1918 - World War I
fy[96:99]
start_96 <- 96
length_96 <- 4

# Temporary intervention only
tmp_96 <- c(rep(0,start_96-1), rep(1,length_96), rep(0, n-start_96-length_96+1))
out_96 <- arimax(log.res, order=order, fix=c(fixed,NA), xtransf=data.frame(tmp_96),
                 transfer=list(c(0,0)))
# Plot total impact
tmp_96.MA0 <- out_96$coef[length(out_96$coef)] # Define MA0 coefficient
tc_96 <- stats::filter(tmp_96, filter=tmp_96.MA0, method='convolution', side=1)
data_96 <- data.frame(
  time_range=1820:2020,
  value=tc_96
)
par(mfrow=c(1,1))
ggplot(data=data_96, aes(x=time_range, y=value)) + geom_line() +
  labs(title="Temporary Intervention Event", x="Year", y="")

# Estimated coefficients
coeftest(out_96)
# Residual analysis
par(mfrow=c(1,1))
plot(out_96$residuals, type='l', ylim=c(-1.5,1.5),
     main="Res. Analysis of LPR WWI Temp. Model", ylab="Residuals")
sde_96 <- sqrt(out_96$sigma2) # Standard deviation of error
cfi_96 <- 2*sde_96 # Confidence interval
abline(h=cfi_96, lty=2) # Upper confidence interval
abline(h=-cfi_96, lty=2) # Lower confidence interval

# 1931-1938 - Great Depression
fy[112:119]
start_112 <- 112
length_112 <- 8

# Temporary intervention only
tmp_112 <- c(rep(0,start_112-1), rep(1,length_112), rep(0, n-start_112-length_112+1))
out_112 <- arimax(log.res, order=order, fix=c(fixed,NA), xtransf=data.frame(tmp_112),
                 transfer=list(c(0,0)))
# Plot total impact
tmp_112.MA0 <- out_112$coef[length(out_112$coef)] # Define MA0 coefficient
tc_112 <- stats::filter(tmp_112, filter=tmp_112.MA0, method='convolution', side=1)
data_112 <- data.frame(
  time_range=1820:2020,
  value=tc_112
)
par(mfrow=c(1,1))
ggplot(data=data_112, aes(x=time_range, y=value)) + geom_line() +
  labs(title="Temporary Intervention Event", x="Year", y="")

# Estimated coeficients
coeftest(out_112)
# Residual analysis
par(mfrow=c(1,1))
plot(out_112$residuals, type='l', ylim=c(-1.5,1.5),
     main="Res. Analysis of LPR Great Depression Temp. Model", ylab="Residuals")
sde_112 <- sqrt(out_112$sigma2) # Standard deviation of error
cfi_112 <- 2*sde_112 # Confidence interval
abline(h=cfi_112, lty=2) # Upper confidence interval
abline(h=-cfi_112, lty=2) # Lower confidence interval

# 1940-1945 - World War II
fy[121:126]
start_121 <- 121
length_121 <- 6

# Temporary intervention only
tmp_121 <- c(rep(0,start_121-1), rep(1,length_121), rep(0, n-start_121-length_121+1))
out_121 <- arimax(log.res, order=order, fix=c(fixed,NA), xtransf=data.frame(tmp_121),
                  transfer=list(c(0,0)))
# Plot total impact
tmp_121.MA0 <- out_121$coef[length(out_121$coef)] # Define MA0 coefficient
tc_121 <- stats::filter(tmp_121, filter=tmp_121.MA0, method='convolution', side=1)
data_121 <- data.frame(
  time_range=1820:2020,
  value=tc_121
)
par(mfrow=c(1,1))
ggplot(data=data_121, aes(x=time_range, y=value)) + geom_line() +
  labs(title="Temporary Intervention Event", x="Year", y="")

# Estimated coefficients
coeftest(out_121)
# Residual analysis
par(mfrow=c(1,1))
plot(out_121$residuals, type='l', ylim=c(-1.5,1.5),
     main="Res. Analysis of LPR World War II Temp. Model", ylab="Residuals")
sde_121 <- sqrt(out_121$sigma2) # Standard deviation of error
cfi_121 <- 2*sde_121 # Confidence interval
abline(h=cfi_121, lty=2) # Upper confidence interval
abline(h=-cfi_121, lty=2) # Lower confidence interval

# 1989-1991 - Immigration Reform and Control Act of 1986
fy[170:172]
start_170 <- 170
length_170 <- 3

# Temporary intervention only
tmp_170 <- c(rep(0,start_170-1), rep(1,length_170), rep(0, n-start_170-length_170+1))
out_170 <- arimax(log.res, order=order, fix=c(fixed,NA), xtransf=data.frame(tmp_170),
                  transfer=list(c(0,0)))
# Plot total impact
tmp_170.MA0 <- out_170$coef[length(out_170$coef)] # Define MA0 coefficient
tc_170 <- stats::filter(tmp_170, filter=tmp_170.MA0, method='convolution', side=1)
data_170 <- data.frame(
  time_range=1820:2020,
  value=tc_170
)
par(mfrow=c(1,1))
ggplot(data=data_170, aes(x=time_range, y=value)) + geom_line() +
  labs(title="Temporary Intervention Event", x="Year", y="")

# Estimated coeficients
coeftest(out_170)
# Residual analysis
par(mfrow=c(1,1))
plot(out_170$residuals, type='l', ylim=c(-1.5,1.5),
     main="Res. Analysis of LPR IRCA Temp. Model", ylab="Residuals")
sde_170 <- sqrt(out_170$sigma2) # Standard deviation of error
cfi_170 <- 2*sde_170 # Confidence interval
abline(h=cfi_170, lty=2) # Upper confidence interval
abline(h=-cfi_170, lty=2) # Lower confidence interval

# NOTE- Measuring recent intervention events lead to a singularity issue.
# Example: Family Separation Policy for annual data.
# Reference Link: https://statisticsglobe.com/r-error-in-solve-system-is-exactly-singular

## Total intervention event impact
# Combine data
data_total <- data.frame(
  time_range <- 1820:2020,
  value <- c(rep(0,start_96-1),
             data_96[start_96:(start_96+length_96-1),2], # WWI
             rep(0,(start_112-1)-(start_96+length_96-1)),
             data_112[start_112:(start_112+length_112-1),2], # Great Depression
             rep(0,(start_121-1)-(start_112+length_112-1)),
             data_121[start_121:(start_121+length_121-1),2], # WWII
             rep(0,(start_170-1)-(start_121+length_121-1)),
             data_170[start_170:(start_170+length_170-1),2], #IRCA
             rep(0,length(fy)-(start_170+length_170-1))
             )
)
# Plot total intervention event impacts
par(mfrow=c(1,1))
ggplot(data=data_total, aes(x=time_range, y=value)) + geom_line() +
  # World War I
  geom_vline(aes(xintercept=1915, color="World War I"), linetype="dashed") +
  geom_vline(aes(xintercept=1918, color="World War I"), linetype="dashed") +
  # Great Depression
  geom_vline(aes(xintercept=1931, color="Great Depression"), linetype="dashed") +
  geom_vline(aes(xintercept=1938, color="Great Depression"), linetype="dashed") +
  # World War II
  geom_vline(aes(xintercept=1940, color="World War II"), linetype="dashed") +
  geom_vline(aes(xintercept=1945, color="World War II"), linetype="dashed") +
  # IRCA
  geom_vline(aes(xintercept=1989, color="IRCA"), linetype="dashed") +
  geom_vline(aes(xintercept=1991, color="IRCA"), linetype="dashed") +
  # Labels
  labs(title="Annual LPR Intervention Event Total Impact", x="Year", y="",
       color="Intervention Events")
