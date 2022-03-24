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
app.monthly <- read.csv("monthly_app_cbp.csv")
app <- app.monthly$Number
mon <- as.Date(app.monthly$Month, "%Y-%m-%d")
# Convert to Month and Year
app.monthly$Month <- as.yearmon(mon)
mon <- app.monthly$Month

##### Plotting Time Series #####
## Time series plot
# Regular plot
par(mfrow=c(1,1))
plot(mon, app, type="l",
     main="U.S. Border Patrol Monthly Apprehensions (FY 2000 - FY 2020)",
     xlab="Month", ylab="Apprehended")
# ggplot2
par(mfrow=c(1,1))
ggplot(data=app.monthly, aes(x=Month, y=Number)) + geom_line() +
  labs(title="U.S. Border Patrol Monthly Apprehensions (FY 2000 - FY 2020)",
       x="Year", y="Number")
# Seasonality is s=12 (monthly)

## Intervention events annotated
# Regular plot
par(mfrow=c(1,1))
plot(mon, app, type="o",
     main="U.S. Border Patrol Monthly Apprehensions (FY 2000 - FY 2020)",
     xlab="Year", ylab="Number")
# Great Recession
abline(v=mon[99], lty=2, col='red') # Dec 2007
abline(v=mon[117], lty=2, col='red') # Jun 2009
# Mexico Temp. Humanitarian Visa
abline(v=mon[232], lty=2, col='green') # Jan 2019
abline(v=mon[235], lty=2, col='green') # Apr 2019
# Legend
legend("topright", c("Great Recession", "Mex. Temp. Hum. Visa"),
       col=c("red", "green"), lty=c(2,2))

# ggplot2
par(mfrow=c(1,1))
ggplot(data=app.monthly, aes(x=Month, y=Number)) + geom_line() + 
  geom_point(alpha=0.5) +
  # Great Recession
  geom_vline(aes(xintercept=mon[99], color="Great Recession", linetype="Great Recession")) +
  geom_vline(aes(xintercept=mon[117], color="Great Recession", linetype="Great Recession")) +
  # Mexico Temp. Humanitarian Visa
  geom_vline(aes(xintercept=mon[232], color="Mex. Temp. Hum. Visa", linetype="Mex. Temp. Hum. Visa")) +
  geom_vline(aes(xintercept=mon[235], color="Mex. Temp. Hum. Visa", linetype="Mex. Temp. Hum. Visa")) +
  # Labels
  labs(title="U.S. Border Patrol Monthly Apprehensions (FY 2000 - FY 2020)",
       x="Year", y="Number", color="Intervention Events",
       linetype='Intervention Events')

# Histogram
par(mfrow=c(1,1))
hist(app, main="Monthly Apprehension Distribution",
     xlab="Apprehensions")
# ggplot2
par(mfrow=c(1,1))
ggplot(app.monthly, aes(x=app)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=20) +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Monthly Apprehension Distribution",
       x="Apprehensions", y="Frequency")

# Descriptive statistics
# Standard (mean, std dev, max, min)
summary(app)
sd(app)
# Boxplot of each month
par(mfrow=c(1,1))
sw.ts <- ts(app,start=c(1999,10), frequency=12)
boxplot(sw.ts~cycle(sw.ts), main="Boxplot of Monthly Apprehensions",
        xlab="Month", ylab="Number",
        names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                "Sep", "Oct", "Nov", "Dec"))

# Check stationarity
par(mfrow=c(1,2))
acf(app,lag.max=60)
pacf(app,lag.max=60)

# Check for constant variance
# Load library for Box-Cox lambda
# Check lambda value
# If lambda=0, a log transformation is needed
BoxCox.lambda(app) # -0.359 - Apply log
# Log transformation applied
log.app <- log(app)

##### Seasonal Difference Applied #####
# Difference order 1 (d = 1)
dlog.app <- diff(log.app, 12) # Seasonal difference
d2log.app <- diff(dlog.app) # Regular difference

# Check for constant mean
# Augmented Dickey-Fuller unit root test
adf.test(d2log.app) # Data is stationary

# ACF and PACF
par(mfrow = c(1,2))
acf(d2log.app, lag.max=60)
pacf(d2log.app,lag.max=60)
# Seasonal model possibilities: SAR(1), SMA(1), SAR(2), SMA(2), SAR(3), SMA(3)

#Plot the time series
par(mfrow=c(1,1))
plot(d2log.app, type="l",
     main="Monthly Apprehensions - Seasonal Difference", ylab="Apprehended")

##### Determine Seasonal Order #####
# Create SARIMA model with no regular order and is SAR(1)
out1 <- arima(d2log.app, order=c(0,0,0),seasonal=list(order=c(1,0,0), period=12))
out1$aic # Model not selected

# Create SARIMA model with no regular order and is SMA(1)
out2 <- arima(d2log.app, order=c(0,0,0), seasonal=list(order=c(0,0,1), period=12))
out2$aic # Model not selected

# Create SARIMA model with no regular order and is SAR(2)
out3 <- arima(d2log.app, order=c(0,0,0), seasonal=list(order=c(2,0,0), period=12))
out3$aic # Model not selected

# Create SARIMA model with no regular order and is SMA(2)
out4 <- arima(d2log.app, order=c(0,0,0), seasonal=list(order=c(0,0,2), period=12))
out4$aic # Model not selected

# Create SARIMA model with no regular order and is SAR(3)
out5 <- arima(d2log.app, order=c(0,0,0), seasonal=list(order=c(3,0,0), period=12))
out5$aic # Model not selected

# Create SARIMA model with no regular order and is SMA(3)
out6 <- arima(d2log.app, order=c(0,0,0), seasonal=list(order=c(0,0,3), period=12))
out6$aic # Model selected

# Apply difference in SARIMA model
out6.1 <- arima(log.app, order=c(0,1,0), seasonal=list(order=c(0,1,3), period=12))
out6.1$aic # Model selected

##### Determine Regular Order #####
# EACF
eacf(out6.1$residuals) # Model cannot be determined

# ACF & PACF (Focus on the first 12 lag only)
par(mfrow=c(1,2))
acf(out6.1$residuals) # MA(11), MA(1)
pacf(out6.1$residuals) # AR(11), AR(1)
# NOTE: Lag 11 is the last significant value. However, too close to the seasonal lag.
# Focus on MA(1) and AR(1)

# ARIMA(0,1,11) x (0,1,3)_12
# MA(11)
out6.1.1 <- arima(log.app, order=c(0,1,11), seasonal=list(order=c(0,1,3), period=12))
out6.1.1$aic # Model not selected (Due to the previous note)

# ARIMA(11,1,0) x (0,1,3)_12
# AR(11)
out6.1.2 <- arima(log.app, order=c(11,1,0), seasonal=list(order=c(0,1,3), period=12))
out6.1.2$aic # Model not selected (Due to the previous note)

# ARIMA(1,1,0) x (0,1,3)_12
# AR(1)
out6.1.3 <- arima(log.app, order=c(1,1,0), seasonal=list(order=c(0,1,3), period=12))
out6.1.3$aic # Model selected

# ARIMA(0,1,1) x (0,1,3)_12
# MA(1)
out6.1.4 <- arima(log.app, order=c(0,1,1), seasonal=list(order=c(0,1,3), period=12))
out6.1.4$aic # Model not selected

##### Remove Non-Significant Coefficients #####
coeftest(out6.1.3)
out6.1.3.1 <- arima(log.app, order=c(1,1,0), seasonal=list(order=c(0,1,3), period=12),
                    fixed=c(NA,NA,0,0))
out6.1.3.1$aic # Model not selected

##### Model Diagnostics #####
# 1) Residual Analysis
# Plot residuals
par(mfrow=c(1,1))
plot(out6.1.3$residuals, type='l', ylim=c(-1,1),
     main="Residual Analysis of Monthly Apprehensions")
sde <- sqrt(out6.1.3$sigma2) # Standard deviation of error
cfi <- 2*sde # Confidence interval
abline(h=cfi, lty=2) # Upper confidence interval
abline(h=-cfi, lty=2) # Lower confidence interval

# Check for outliers
locate.outliers(out6.1.3$residuals, par=coefs2poly(out6.1.3))

# ACF and PACF
par(mfrow = c(1,2))
pacf(out6.1.3$residuals)
acf(out6.1.3$residuals)
# Residuals appears to be white noise (except at lag 11)
# Need to make final determination using Box-Ljung test

# Box-Ljung test
Box.test(out6.1.3$residuals, lag=60, type="Ljung") # Residuals are white noise

# 2) Check for Overspecification
# Go to "Remove Non-Significant Coefficients" section

# 3) Check for Stationary through AR Roots
polyroot(c(1, -out6.1.3$coef[1]))
abs(polyroot(c(1, -out6.1.3$coef[1])))
# Root not close to 1

# 4) Check for Model Redundancy through AR and MA Roots
# No matching MA and SAR roots

##### Intervention Events #####
source('FUNCTIONS_Intervention_Events.R')
# Prepare variables
x <- ts(app, start=mon[1], frequency=1)
n <- length(x)
order <- c(1,1,0)
seasonal <- list(order=c(0,1,3), period=12)
fixed <- c(NA,NA,NA,NA)

# Dec 2007-Jun 2009 - Great Recession
mon[99:117]
length(mon[99:117]) # Range of event
tp99 <- intervention_events(ls=app, outlier=99, range=19, start=mon[1],
                            order=order, fixed=fixed, seasonal=seasonal)
tp99$int_event[[1]] # None
# Plot total impact
plot_ie(99, tp99$int_event[[1]], tp99$tmp, tp99$perm, tp99$model[[1]],
        mon[1:252], "Month")[[1]]
# Estimated coeficients
coeftest(tp99$model[[1]])
# Residual analysis
par(mfrow=c(1,1))
plot(tp99$model[[1]]$residuals, type='l', ylim=c(-1.0,1.0),
     main="Res. Analysis of Monthly App. Great Recession Model",
     ylab="Residuals")
sde_99 <- sqrt(tp99$model[[1]]$sigma2) # Standard deviation of error
cfi_99 <- 2*sde_99 # Confidence interval
abline(h=cfi_99, lty=2) # Upper confidence interval
abline(h=-cfi_99, lty=2) # Lower confidence interval

# Jan 2019-Apr 2019 - Mexico Temp. Humanitarian Visa
mon[232:235]
start_232 <- 232
length_232 <- 4

# Temporary intervention only
tmp_232 <- c(rep(0,start_232-1), rep(1,length_232), rep(0, n-start_232-length_232+1))
out_232 <- arimax(log.app, order=order, seasonal=seasonal, fixed=c(fixed,NA),
                  xtransf=data.frame(tmp_232), transfer=list(c(0,0)))

# Plot total impact
tmp_232.MA0 <- out_232$coef[length(out_232$coef)] # Define MA0 coefficient
tc_232 <- stats::filter(tmp_232, filter=tmp_232.MA0, method='convolution', side=1)
data_232 <- data.frame(
  time_range=mon,
  value=tc_232
)
par(mfrow=c(1,1))
ggplot(data=data_232, aes(x=time_range, y=value)) + geom_line() +
  labs(title="Temporary Intervention Event", x="Year", y="")
# Estimated coefficients
coeftest(out_232)
# Residual analysis
par(mfrow=c(1,1))
plot(out_232$residuals, type='l', ylim=c(-1.5,1.5),
     main="Res. Analysis of Monthly App. Mex. Visa Temp. Model", ylab="Residuals")
sde_232 <- sqrt(out_232$sigma2) # Standard deviation of error
cfi_232 <- 2*sde_232 # Confidence interval
abline(h=cfi_232, lty=2) # Upper confidence interval
abline(h=-cfi_232, lty=2) # Lower confidence interval

## Total intervention event impact
# Plot total intervention event impacts
par(mfrow=c(1,1))
ggplot(data=data_232, aes(x=time_range, y=value)) + geom_line() +
  # Mexico Temp. Humanitarian Visa
  geom_vline(aes(xintercept=mon[232], color="Mex. Temp. Hum. Visa"), linetype="dashed") +
  geom_vline(aes(xintercept=mon[235], color="Mex. Temp. Hum. Visa"), linetype="dashed") +
  # Labels
  labs(title="Monthly App. Intervention Event Total Impact", x="Year", y="",
       color="Intervention Events")
