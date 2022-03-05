# Perform intervention event analysis for each outlier
  # ls = List of the numbers being looked at
  # outlier = The index of the first outlier from ls
  # range = The outlier range for certain events that last longer than a unit mark of time (e.g. year or month)
  # start = Beginning time point
  # order = ARIMA order
  # fixed = Fixed parameters
  # diff_reg = Regular difference applied needed (if not added in the ARIMA function)
  # diff_season = Seasonal difference applied needed (if not added in the ARIMA function)
  # seasonal = SARIMA order (seasonality)

intervention_events <- function(ls, outlier, range, start, order, fixed,
                                seasonal=list(order=c(0,0,0), period=12),
                                diff_reg="No", diff_season=0) {
  # Prepare variables
  x <- ts(ls, start=start, frequency=1)
  lx <- log(x) # With outlier
  n <- length(x)
  
  # Apply seasonal difference for cases applied before creating the ARIMA model
  if (diff_season > 0) {
    lx <- diff(lx, diff_season)
  }
  
  # Apply regular difference for cases applied before creating the ARIMA model
  if (diff_reg == "Yes") {
    lx <- diff(lx)
    n <- length(lx)
  }
  
  # Fixed parameter
  fixed_perm <- c(fixed, NA)
  fixed_temp <- c(fixed, NA, NA)
  fixed_both <- c(fixed, NA, NA, NA)
  
  # Temporary intervention
  tmp <- ts(c(rep(0, outlier-1), rep(1, range), rep(0, n-(outlier-1+range))), start=start)
  # Permanent intervention
  perm <- ts(c(rep(0, outlier-1), rep(1, n-(outlier-1))), start=start)
  
  # No intervention
  m00 <- arimax(lx, order=order, fixed=fixed, seasonal=seasonal, method="ML")
  # Permanent intervention
  m01 <- arimax(lx, order=order, fixed=fixed_perm, seasonal=seasonal, xtransf=data.frame(perm),
                transfer=list(c(0,0)), method="ML")
  # Temporary intervention
  m10 <- arimax(lx, order=order, fixed=fixed_temp, seasonal=seasonal, xtransf=data.frame(tmp),
                transfer=list(c(1,0)), method="ML")
  # Temporary & permanent intervention
  m11 <- arimax(lx, order=order, fixed=fixed_both, seasonal=seasonal,
                xtransf=data.frame(tmp, perm),
                transfer=list(c(1,0), c(0,0)), method="ML")
  
  # Select the lowest AIC for the intervention event
  aic <- c(m00$aic, m01$aic, m10$aic, m11$aic)
  model.results <- list('None', 'Permanent', 'Temporary', 'Permanent & Temporary')
  model <- list(m00, m01, m10, m11)
  index <- which.min(aic)
  
  return_list <- list(model=model[index], int_event=model.results[index],
                      tmp=tmp, perm=perm, models=model)
  return(return_list)
}

# Plotting the intervention event for each outlier
  # outlier = The index of the outlier
  # impact = A string value that is either "None", "Permanent", "Temporary", or "Permanent & Temporary"
  # tmp = Temporary intervention
  # perm = Permanent intervention
  # model = Intervention model
  # order = ARIMA order
  # time_range = A range of time points (e.g. years, months)
  # time_axis = A string value stating the time point (e.g. "Year", "Month")

plot_ie <- function(outlier, impact, tmp, perm, model, time_range, time_axis) {
  # None
  if (impact == "None") {
    return("No impacts to plot")
  }
  
  # Temporary
  if (impact == "Temporary") {
    coef <- model$coef
    tmp.AR1 <- coef[length(coef)-1]
    tmp.MA0 <- coef[length(coef)]
    tc <- stats::filter(tmp, filter=tmp.AR1, method='recursive', side=1)*tmp.MA0
    data <- data.frame(
      time_range=time_range,
      value=tc
    )
    ie <- ggplot(data=data, aes(x=time_range, y=value)) + geom_line() +
      labs(title="Temporary Intervention Event", x=time_axis, y="") # What is the y-axis?
    return(list(ie, data))
  }
  
  # Permanent
  if (impact == "Permanent") {
    coef <- model$coef
    perm.MA0 <- coef[length(coef)]
    ls <- ts(c(rep(0, outlier-1), rep(perm.MA0, length(time_range)-(outlier-1))),
             start=c(time_range[1])) 
    data <- data.frame(
      time_range=time_range,
      value=ls
    )
    ie <- ggplot(data=data, aes(x=time_range, y=value)) + geom_line() +
      labs(title="Permanent Intervention Event", x=time_axis, y="") # What is the y-axis?
    return(list(ie, data))
  }
  
  # Permanent & Temporary
  if (impact == "Permanent & Temporary") {
    coef <- model$coef
    # Temporary change
    tmp.AR1 <- coef[length(coef)-2]
    tmp.MA0 <- coef[length(coef)-1]
    tc <- stats::filter(tmp, filter=tmp.AR1, method='recursive', side=1)*tmp.MA0
    # Permanent (Level shift)
    perm.MA0 <- coef[length(coef)]
    ls <- ts(c(rep(0, outlier-1), rep(perm.MA0, length(time_range)-(outlier-1))),
             start=c(time_range[1]))
    # Dataframe
    data <- data.frame(
      time_range=time_range,
      value=tc+ls
    )
    ie <- ggplot(data=data, aes(x=time_range, y=value)) + geom_line() +
      labs(title="Permanent & Temporary Intervention Event",
           x=time_axis, y="") # What is the y-axis?
    return(list(ie, data))
  }
}