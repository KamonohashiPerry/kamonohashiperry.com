library(tidyverse)
library(urca)
library(fpp)
library(vars)
library(ggfortify)

#import timeseries data set
monthly_data <- data.frame(read_csv("USJ_desney_monthly.csv"))
monthly_data <- monthly_data %>% mutate(month = as.Date(paste(month,"-01",sep="")))
ts_monthly_data <- ts(monthly_data[, 2:3], start = c(2004,1), freq = 12)

autoplot(ts_monthly_data, facets = T)


#ADF test
summary(ur.df(ts_monthly_data[ , 1], type = "drift"))
summary(ur.df(ts_monthly_data[ , 2], type = "drift"))

#mutual correlation
autoplot(ccf(ts_monthly_data[ , 1],ts_monthly_data[ , 2], plot = F))


#log difference
ndiffs(log(ts_monthly_data))
log_diff_monthly_data <-  diff(log(ts_monthly_data))


#ADF test
summary(ur.df(log_diff_monthly_data[ , 1], type = "drift"))
summary(ur.df(log_diff_monthly_data[ , 2], type = "drift"))

select_result <- VARselect(log_diff_monthly_data, lag.max = 10, type = "const")
select_result

select_result$selection[1]

var_bestorder <- VAR(y = log_diff_monthly_data,
                     type = "const",
                     p = select_result$selection[1])

summary(var_bestorder)


predict(var_bestorder, n.ahead = 4)

autoplot(predict(var_bestorder, n.ahead = 8,
          ts.colour = 1,
          predict.colour = 1,
          predict.linetype = 'dashed'))

#Granger Causality
causality(var_bestorder, cause = "ディズニーランド")
causality(var_bestorder, cause = "USJ")


#Inpulse Response Function
irf_USJ <- irf(
  var_bestorder,
  impulse = "USJ",
  response = c("USJ", "ディズニーランド"),
  n.ahead = 12,
  boot = T
)

plot(irf_USJ)

#variance decomposition
plot(fevd(var_bestorder, n.ahead = 12))


