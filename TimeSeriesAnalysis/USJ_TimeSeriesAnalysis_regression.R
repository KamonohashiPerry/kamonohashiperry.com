library(tidyverse)
library(urca)
library(lmtest)
library(prais)
library(ggfortify)
library(gridExtra)

#import timeseries data set
monthly_data <- data.frame(read_csv("USJ_desney_monthly.csv"))
monthly_data <- monthly_data %>% mutate(month = as.Date(paste(month,"-01",sep="")))
ts_monthly_data <- ts(monthly_data[, 2:3], start = c(2004,1), freq = 12)

#model constraction
mod_ols <- lm(ts_monthly_data[, 1] ~ ts_monthly_data[, 2])

summary(mod_ols)

#DW statistics
resid_ols <- mod_ols$residuals
dw <- sum(diff(resid_ols)^2) / sum((resid_ols)^2)
dw

dwtest(mod_ols)

#ADF test
summary(ur.df(ts_monthly_data[, 1], type = "none"))

summary(ur.df(ts_monthly_data[, 2], type = "none"))


#Prais-Winsten法
#residual
resid_ols <- mod_ols$residuals

mod_resid <- lm(resid_ols[-1] ~ resid_ols[-nrow(ts_monthly_data)] - 1)
ro <- as.numeric(mod_resid$coefficients)
ro

y_trans_1 <- sqrt(1 - ro^2)*ts_monthly_data[1,1]
x_trans_1 <- sqrt(1 - ro^2)*ts_monthly_data[1,2]
psi_trans_1 <- sqrt(1 - ro^2)

y_trans_2 <- ts_monthly_data[-1,1] - ro*ts_monthly_data[-nrow(ts_monthly_data),1]
x_trans_2 <- ts_monthly_data[-1,2] - ro*ts_monthly_data[-nrow(ts_monthly_data),2]
psi_trans_2 <- rep(1 - ro, nrow(ts_monthly_data) -1)

y_trans_all <- c(y_trans_1, y_trans_2)
x_trans_all <- c(x_trans_1, x_trans_2)
psi_trans_all <- c(psi_trans_1, psi_trans_2)

mod_gls_hand <- lm(y_trans_all ~ psi_trans_all + x_trans_all -1)
summary(mod_gls_hand)

mod_gls_PW <- prais.winsten(monthly_data$USJ ~ monthly_data$ディズニーランド,
                            data = monthly_data, iter = 1)
mod_gls_PW


#Co-integration
summary(ca.po(monthly_data[,-1], demean = "none"))


