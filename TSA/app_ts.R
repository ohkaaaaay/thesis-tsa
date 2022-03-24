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
app.yearly <- read.csv("app.csv")
app <- app.yearly$Number
fy <- app.yearly$Year

##### Plotting Time Series #####
## Time series plot
# Regular plot
par(mfrow=c(1,1))
plot(fy, app, type="l", main="Annual Apprehensions (FY 1925 - FY 2020)",
     xlab="Year", ylab="Number")
# ggplot2
par(mfrow=c(1,1))
ggplot(data=app.yearly, aes(x=Year, y=Number)) + geom_line() +
  labs(title="Annual Apprehensions (FY 1925 - FY 2020)", x="Year", y="Number")

## Intervention events annotated
# Regular plot
par(mfrow=c(1,1))
plot(fy, app, type="o", main="Annual Apprehensions (FY 1925 - FY 2018)",
     xlab="Year", ylab="Number")
# Operation Wetback
abline(v=1953, lty=2, col='black')
abline(v=1954, lty=2, col='black')
# IRCA
abline(v=1986, lty=2, col='red')
# IIRIRA
abline(v=1997, lty=2, col='blue')
# 9/11 Attacks
abline(v=2001, lty=2, col='green')
# Legend
legend("topleft", c("Operation Wetback", "IRCA", "IRIRA", "9/11 Attacks"),
       col=c("black", "red", "blue", "green"), lty=c(2,2))

# ggplot2
par(mfrow=c(1,1))
ggplot(data=app.yearly, aes(x=Year, y=Number)) + geom_line() + 
  geom_point(alpha=0.5) +
  # Operation Wetback
  geom_vline(aes(xintercept=1953, color="Operation Wetback", linetype="Operation Wetback")) +
  geom_vline(aes(xintercept=1954, color="Operation Wetback", linetype="Operation Wetback")) +
  # IRCA
  geom_vline(aes(xintercept=1986, color="IRCA", linetype="IRCA")) +
  # IIRIRA
  geom_vline(aes(xintercept=1997, color="IIRIRA", linetype="IIRIRA")) +
  # 9/11 Attacks
  geom_vline(aes(xintercept=2001, color="9/11 Attacks", linetype="9/11 Attacks")) +
  # Labels
  labs(title="Annual Apprehensions (FY 1925 - FY 2020)",
       x="Year", y="Number", color="Intervention Events",
       linetype="Intervention Events")

# Histogram
par(mfrow=c(1,1))
hist(app, main="Annual Apprehension Distribution",
     xlab="Apprehensions")
# ggplot2
par(mfrow=c(1,1))
ggplot(app.yearly, aes(x=app)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey", bins=20) +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Annual Apprehension Distribution",
       x="Apprehensions", y="Frequency")

# Descriptive statistics
# Standard (mean, std dev, max, min)
summary(app)
sd(app)

# Check stationarity
par(mfrow=c(1,2))
acf(app)
pacf(app)

# Check for constant variance
# Load library for Box-Cox lambda
# Check lambda value
# If lambda=0, a log transformation is needed
BoxCox.lambda(app) # 0.139 - Apply log
# Log transformation applied
log.app <- log(app)

# Check for constant mean
# Augmented Dickey-Fuller unit root test
adf.test(log.app) # Data is not stationary
# Difference log data
dlog.app <- diff(log.app)
adf.test(dlog.app) # Data is stationary

# Plot stationary dataset
par(mfrow=c(1,1))
plot(fy[1:95], dlog.app, type="l", main="Log & Difference of Apprehension",
     xlab="Year", ylab="Number")

##### Determine Model Order #####
# EACF
eacf(dlog.app) # ARMA(1,11)

# ACF & PACF
par(mfrow=c(1,2))
acf(dlog.app) # MA(12)
pacf(dlog.app) # AR(11)

# Model Build
# ARMA(1,11)
out1 <- arima(dlog.app, order=c(1, 0, 11))
out1$aic # Model not selected
# MA(12)
out2 <- arima(dlog.app, order=c(0, 0, 12))
out2$aic # Model not selected
# AR(11)
out3 <- arima(dlog.app, order=c(11, 0, 0))
out3$aic # Model selected

# Apply difference in model build
# AR(11)
out3.1 <- arima(log.app, order = c(11, 1, 0))
out3.1$aic # Model not selected

##### Remove Non-Significant Coefficients #####
coeftest(out3)
out3.2 <- arima(dlog.app, order=c(11, 0, 0),
                fixed=c(NA,0,0,0,0,0,0,0,0,0,NA,0))
out3.2$aic # Model selected

##### Model Diagnostics #####
# 1) Residual Analysis
# Plot residuals
par(mfrow=c(1,1))
plot(out3.2$residuals, type='l', ylim=c(-1.2,1.2),
     main="Residual Analysis of Apprehensions")
sde <- sqrt(out3.2$sigma2) # Standard deviation of error
cfi <- 2*sde # Confidence interval
abline(h=cfi, lty=2) # Upper confidence interval
abline(h=-cfi, lty=2) # Lower confidence interval

# Check for outliers
locate.outliers(out3.2$residuals,
                par=coefs2poly(out3.2))

# ACF and PACF
par(mfrow = c(1,2))
pacf(out3.2$residuals)
acf(out3.2$residuals)
# Residuals appears to be white noise
# Need to make final determination using Box-Ljung test

# Box-Ljung test
Box.test(out3.2$residuals, lag=12, type="Ljung")
# Residuals are white noise

# 2) Check for Overspecification
# Go to "Remove Non-Significant Coefficients" section

# 3) Check for Stationary through AR Roots
polyroot(c(1, -out3.2$coef[1:11]))
abs(polyroot(c(1, -out3.2$coef[1:11])))
# Root somewhat close to 1
# But a Augmented Dickey-Fuller unit root test determines stationarity
adf.test(dlog.app)

# 4) Check for Model Redundancy through AR and MA Roots
# No MA roots

##### Intervention Events #####
source('FUNCTIONS_Intervention_Events.R')
order <- c(11,0,0)
fixed <- c(NA,0,0,0,0,0,0,0,0,0,NA,0)
diff_reg <- "Yes"

# 1953-1954 - Operation Wetback
fy[29:30] # Time point of outlier
tp29 <- intervention_events(ls=app, outlier=29, range=2, start=fy[1],
                            order=order, fixed=fixed, diff_reg=diff_reg)
tp29$int_event[[1]] # Temporary
# Plot total impact
plot_ie(29, tp29$int_event[[1]], tp29$tmp, tp29$perm, tp29$model[[1]],
        fy[1:95], "Year")[[1]] # time_range is 95 due to dataset being differenced (diff_reg)
# Estimated coeficients
coeftest(tp29$model[[1]])
# Residual analysis
par(mfrow=c(1,1))
plot(tp29$model[[1]]$residuals, type='l', ylim=c(-1.0,1.0),
     main="Res. Analysis of Annual App. Operation Wetback Temp. Model",
     ylab="Residuals")
sde_29 <- sqrt(tp29$model[[1]]$sigma2) # Standard deviation of error
cfi_29 <- 2*sde_29 # Confidence interval
abline(h=cfi_29, lty=2) # Upper confidence interval
abline(h=-cfi_29, lty=2) # Lower confidence interval

# 1986 - Immigration Reform and Control Act of 1986
fy[62]
tp62 <- intervention_events(ls=app, outlier=62, range=1, start=fy[1],
                            order=order, fixed=fixed, diff_reg=diff_reg)
tp62$int_event[[1]] # None
# Plot total impact
plot_ie(62, tp62$int_event[[1]], tp62$tmp, tp62$perm, tp62$model[[1]],
        fy[1:95], "Year")[[1]] # time_range is 95 due to dataset being differenced (diff_reg)
# Estimated coeficients
coeftest(tp62$model[[1]])
# Residual analysis
par(mfrow=c(1,1))
plot(tp62$model[[1]]$residuals, type='l', ylim=c(-1.5,1.5),
     main="Res. Analysis of Annual App. IRCA Perm. Model",
     ylab="Residuals")
sde_62 <- sqrt(tp62$model[[1]]$sigma2) # Standard deviation of error
cfi_62 <- 2*sde_62 # Confidence interval
abline(h=cfi_62, lty=2) # Upper confidence interval
abline(h=-cfi_62, lty=2) # Lower confidence interval

# 1997 - Illegal Immigration Reform and Immigrant Responsibility Act of 1996
fy[73]
tp73 <- intervention_events(ls=app, outlier=73, range=1, start=fy[1],
                            order=order, fixed=fixed, diff_reg=diff_reg)
tp73$int_event[[1]] # None
# Plot total impact
plot_ie(73, tp73$int_event[[1]], tp73$tmp, tp73$perm, tp73$model[[1]],
        fy[1:95], "Year")[[1]] # time_range is 94 due to dataset being differenced (diff_reg)
# Estimated coeficients
coeftest(tp73$model[[1]])
# Residual analysis
par(mfrow=c(1,1))
plot(tp73$model[[1]]$residuals, type='l', ylim=c(-1.5,1.5),
     main="Res. Analysis of Annual App. IIRIRA Attacks Model",
     ylab="Residuals")
sde_73 <- sqrt(tp73$model[[1]]$sigma2) # Standard deviation of error
cfi_73 <- 2*sde_73 # Confidence interval
abline(h=cfi_73, lty=2) # Upper confidence interval
abline(h=-cfi_73, lty=2) # Lower confidence interval

# 2001 - September 11 Attacks
fy[77]
tp77 <- intervention_events(ls=app, outlier=78, range=1, start=fy[1],
                            order=order, fixed=fixed, diff_reg=diff_reg)
tp77$int_event[[1]] # None
# Plot total impact
plot_ie(77, tp77$int_event[[1]], tp77$tmp, tp77$perm, tp77$model[[1]],
        fy[1:95], "Year")[[1]] # time_range is 94 due to dataset being differenced (diff_reg)
# Estimated coeficients
coeftest(tp77$model[[1]])
# Residual analysis
par(mfrow=c(1,1))
plot(tp77$model[[1]]$residuals, type='l', ylim=c(-1.5,1.5),
     main="Res. Analysis of Annual App. 9/11 Attacks Perm. Model",
     ylab="Residuals")
sde_77 <- sqrt(tp77$model[[1]]$sigma2) # Standard deviation of error
cfi_77 <- 2*sde_77 # Confidence interval
abline(h=cfi_77, lty=2) # Upper confidence interval
abline(h=-cfi_77, lty=2) # Lower confidence interval

# NOTE- Measuring recent intervention events lead to a singularity issue.
# Example: Family Separation Policy for annual data.
# Reference Link: https://statisticsglobe.com/r-error-in-solve-system-is-exactly-singular

## Total intervention event impact
# Retrieve data
data_29 <- plot_ie(29, tp29$int_event[[1]], tp29$tmp, tp29$perm, tp29$model[[1]],
                   fy[1:95], "Year")[[2]]
# Combine data
data_total <- data.frame(
  time_range <- fy[2:96],
  value <- c(data_29[,2])
  # Other events had no impact
)
# Plot total intervention event impacts
par(mfrow=c(1,1))
ggplot(data=data_total, aes(x=time_range, y=value)) + geom_line() +
  # Operation Wetback
  geom_vline(aes(xintercept=fy[29], color="Operation Wetback"), linetype="dashed") +
  geom_vline(aes(xintercept=fy[30], color="Operation Wetback"), linetype="dashed") +
  # IRCA
  #geom_vline(aes(xintercept=fy[62], color="IRCA"), linetype="dashed") +
  # Labels
  labs(title="Annual App. Intervention Event Total Impact", x="Year", y="",
       color="Intervention Events")
