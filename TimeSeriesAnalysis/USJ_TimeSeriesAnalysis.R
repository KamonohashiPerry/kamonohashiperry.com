library(tidyverse)
library(xts)
library(forecast)
library(urca)
library(ggfortify)
library(tseries)

#import timeseries data set
monthly_data <- data.frame(read_csv("usj_monthly.csv"))
monthly_data <- monthly_data %>% mutate(month = as.Date(paste(month,"-01",sep="")))
ts_monthly_data <- ts(monthly_data$USJ, start = c(2004,1), freq = 12)


#transform data to xts class
monthly_data_xts <- as.xts(read.zoo(monthly_data))

#graph draw
autoplot(monthly_data_xts,
     main = "USJ Timeseries Data in Google Trend",
     xlab = "month",
     ylab = "trend index")

#unit root test
summary(ur.kpss(log(monthly_data_xts)))

#the number of diff
ndiffs(log(monthly_data_xts))

#log transformation
log_monthly_data_xts <- log(monthly_data_xts)
#figure
ggtsdisplay(log_monthly_data_xts, main = "log series")


#log diff
log_diff_monthly_data_xts <- diff(log_monthly_data_xts)
#figure
ggtsdisplay(log_diff_monthly_data_xts, main = "log difference series")


#seasonality
ggsubseriesplot(ts_monthly_data)

#seasonal diff
seas_log_diff <- diff(log_diff_monthly_data_xts,
                      lag = frequency(log_diff_monthly_data_xts))
#figure
ggtsdisplay(seas_log_diff, main = "seasonal diff series")

acf(seas_log_diff, plot = F, lag.max = 12, na.action = na.pass)

#ARIMA
train <- window(log(ts_monthly_data), end = c(2015, 12))
test  <- window(log(ts_monthly_data), start = c(2016, 1))

sarima <- auto.arima(y = train,
                     ic = "aic",
                     max.order = 7,
                     stepwise = F,
                     approximation = F,
                     parallel = T,
                     num.cores = 4
                     )

#auto correlation check
checkresiduals(sarima)

#residual standardization check
jarque.bera.test(resid(sarima))


sarima_f <- forecast(sarima,
                     h = 12,
                     level = c(95, 70))
sarima_f
autoplot(sarima_f, predict.colour = 1, main = "ARIMA Prediction")

#naive prediction
naive_f_mean <- meanf(train, h = 12)
naive_f_latest <- rwf(train, h = 12)

#Valuation of prediction
sarima_rmse <- sqrt(sum(sarima_f$mean - test)^2)/length(sarima_f$mean)
sarima_rmse

accuracy(sarima_f, x = test)
accuracy(sarima_f, x = test)["Test set", "RMSE"]

accuracy(naive_f_mean, x = test)["Test set", "RMSE"]
accuracy(naive_f_latest, x = test)["Test set", "RMSE"]

