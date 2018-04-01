library(dlm)
library(KFAS)
library(tidyverse)
library(ggfortify)
library(reshape)

#import timeseries data set
monthly_data <- data.frame(read_csv("usj_monthly.csv"))
monthly_data <- monthly_data %>% mutate(month = as.Date(paste(month,"-01",sep="")))
ts_monthly_data <- ts(monthly_data$USJ, start = c(2004,1), freq = 12)

kfLocalLevel <- function(y, #観測値
                         mu_pre, #前期の状態
                         P_pre, #前期の状態の予測誤差の分散
                         sigma_w, #過程誤差の分散
                         sigma_v #観測誤差の分散
                         ){
                          ## Step1 Prediction
                          mu_forecast <- mu_pre #状態の予測値
                          P_forecast <- P_pre + sigma_w #状態の予測値の分散
                          y_forecast <- mu_forecast #観測値の予測値
                          F <- P_forecast + sigma_v #観測値の予測誤差の分散
                          
                          ## Filtering
                          K <- P_forecast / (P_forecast + sigma_v) #カルマンゲイン
                          y_resid <- y - y_forecast #観測値の予測残差
                          mu_filter <- mu_forecast + K * y_resid #補正後の状態
                          P_filter <- (1 - K) * P_forecast #フィルタ化推定量の分散
                          
                          # Stock Result
                          result <- data.frame(
                                                mu_filter = mu_filter,
                                                P_filter  = P_filter,
                                                y_resid   = y_resid,
                                                F = F,
                                                K = K
                                                )
                          return(result)
                        }

#サンプルサイズ
N <- length(ts_monthly_data)

#状態の推定値
mu_filter <- numeric(N)

#状態の初期値は0とする。
mu_zero <- 0

mu_filter <- c(mu_zero, mu_filter)

#状態の予測誤差の分散
P_filter <- numeric(N)

#状態の予測誤差の分散の初期値を10000000にする。
P_zero <- 10000000
P_filter <- c(P_zero, P_filter)

#観測値の予測誤差
y_resid <- numeric(N)

#観測値の予測誤差の分散
F <- numeric(N)

#カルマンゲイン
K <- numeric(N)

#過程誤差の分散
sigma_w <- 1000

#観測誤差の分散
sigma_v <- 10000

for(i in 1:N){
  kekka <- kfLocalLevel(
                          y = ts_monthly_data[i],
                          mu_pre = mu_filter[i],
                          P_pre = P_filter[i],
                          sigma_w = sigma_w,
                          sigma_v = sigma_v
                        )
  mu_filter[i + 1] <- kekka$mu_filter
  P_filter[i + 1]  <- kekka$P_filter
  y_resid[i]       <- kekka$y_resid
  F[i] <- kekka$F
  K[i] <- kekka$K
}


data_vis <- data.frame(month = monthly_data$month,
                       usj = monthly_data$USJ,
                       filter = mu_filter[-1])
data_vis <- melt(data_vis, id.vars = "month")

ggplot(data = data_vis,
       aes(x = month, y=value, colour=variable)) + geom_line()


#対数尤度(ver1)
sum(log(dnorm(y_resid,
              mean = 0,
              sd = sqrt(F))))

#対数尤度(ver2)
-1 * (N/2) * log(2 * pi) - 1/2 * sum(log(F) + y_resid^2 / F)

1/2 * sum(log(F) + y_resid^2 / F)


#Caluculate Likelihood
calkLogLik <- function(sigma){
                              sigma_w <- exp(sigma[1])
                              sigma_v <- exp(sigma[2])
                              #変数の定義など
                              N <- length(ts_monthly_data)
                              mu_filter <- numeric(N)
                              mu_zero <- 0
                              mu_filter <- c(mu_zero, mu_filter)
                              P_filter <- numeric(N)
                              P_zero <- 10000000
                              P_filter <- c(P_zero, P_filter)
                              y_resid <- numeric(N)
                              F <- numeric(N)
                              K <- numeric(N)
                              
                              #カルマンフィルタの実行
                              for(i in 1:N){
                                kekka <- kfLocalLevel(
                                                        y = ts_monthly_data[i],
                                                        mu_pre = mu_filter[i],
                                                        P_pre = P_filter[i],
                                                        sigma_w = sigma_w,
                                                        sigma_v = sigma_v
                                                      )
                                mu_filter[i + 1] <- kekka$mu_filter
                                P_filter[i + 1]  <- kekka$P_filter
                                y_resid[i]       <- kekka$y_resid
                                F[i] <- kekka$F
                                K[i] <- kekka$K
                              }
                              return(1/2 * sum(log(F) + y_resid^2 / F))
}


best_sigma <- optim(calkLogLik, par = c(1,1), method = "L-BFGS")
exp(best_sigma$par)


#最適値の過程誤差の分散
sigma_w <- exp(best_sigma$par)[1]

#最適値の観測誤差の分散
sigma_v <- exp(best_sigma$par)[2]

#サンプルサイズ
N <- length(ts_monthly_data)
#状態の推定値
mu_filter <- numeric(N)
#状態の初期値は0とする。
mu_zero <- 0
mu_filter <- c(mu_zero, mu_filter)
#状態の予測誤差の分散
P_filter <- numeric(N)
#状態の予測誤差の分散の初期値を10000000にする。
P_zero <- 10000000
P_filter <- c(P_zero, P_filter)
#観測値の予測誤差
y_resid <- numeric(N)
#観測値の予測誤差の分散
F <- numeric(N)
#カルマンゲイン
K <- numeric(N)

for(i in 1:N){
              kekka <- kfLocalLevel(
                                    y = ts_monthly_data[i],
                                    mu_pre = mu_filter[i],
                                    P_pre = P_filter[i],
                                    sigma_w = sigma_w,
                                    sigma_v = sigma_v
              )
              mu_filter[i + 1] <- kekka$mu_filter
              P_filter[i + 1]  <- kekka$P_filter
              y_resid[i]       <- kekka$y_resid
              F[i] <- kekka$F
              K[i] <- kekka$K
}


#対数尤度(ver2)
-1 * (N/2) * log(2 * pi) - 1/2 * sum(log(F) + y_resid^2 / F)

1/2 * sum(log(F) + y_resid^2 / F)


data_vis <- data.frame(month = monthly_data$month,
                       usj = monthly_data$USJ,
                       filter = mu_filter[-1])
data_vis <- melt(data_vis, id.vars = "month")

ggplot(data = data_vis,
       aes(x = month, y=value, colour=variable)) + geom_line()


#平滑化
smoothLocalLevel <- function(mu_filtered, P_filtered, r_post, s_post,
                             F_post, y_resid_post, K_post){
                              #状態平滑化漸化式
                              r <- y_resid_post/F_post + (1- K_post) * r_post #状態平滑化漸化式のパラメータ
                              mu_smooth <- mu_filtered + P_filtered * r #平滑化状態
                              #状態分散平滑化漸化式
                              s <- 1/F_post + (1 - K_post)^2 * s_post #状態分散平滑化漸化式のパラメータ
                              P_smooth <- P_filtered - P_filtered^2 * s #平滑化状態分散
                              #結果の格納
                              result <- data.frame(
                                mu_smooth = mu_smooth,
                                P_smooth = P_smooth,
                                r = r,
                                s = s
                              )
                              return(result)
}

#平滑化状態
mu_smooth <- numeric(N + 1)
#平滑化状態分散
P_smooth <- numeric(N + 1)
#漸化式のパラメータ
r <- numeric(N)
s <- numeric(N)
#最後のデータはフィルタリングの結果とスムージングの結果が一致する
mu_smooth[N + 1] <- mu_filter[N + 1]
P_smooth[N + 1] <- P_filter[N + 1]


for(i in N:1){
              kekka <- smoothLocalLevel(
                                        mu_filter[i],
                                        P_filter[i],
                                        r[i],
                                        s[i],
                                        F[i],
                                        y_resid[i],
                                        K[i]
                                      )
              mu_smooth[i] <- kekka$mu_smooth
              P_smooth[i]  <- kekka$P_smooth
              r[i - 1] <- kekka$r
              s[i - 1] <- kekka$s
}

data_vis <- data.frame(month = monthly_data$month,
                       usj = monthly_data$USJ,
                       filter = mu_filter[-1],
                       smooth = mu_smooth[-1])
data_vis <- melt(data_vis, id.vars = "month")

ggplot(data = data_vis,
       aes(x = month, y=value, colour=variable)) + geom_line()

##散漫カルマンフィルタ
#状態の推定値
mu_diffuse_filter <- numeric(N + 1)
#状態の予測誤差の分散
P_diffuse_filter <- numeric(N + 1)
#散漫初期化を用いる際の1時点目のフィルタ化推定量
mu_diffuse_filter[2] <- ts_monthly_data[1]
P_diffuse_filter[2]  <- sigma_v

#観測値の予測残渣
y_resid_diffuse <- numeric(N)
#観測値の予測誤差の分散
F_diffuse <- numeric(N)
#カルマンゲイン
K_diffuse <- numeric(N)

for(i in 2:N){
  kekka <- kfLocalLevel(
    y = ts_monthly_data[i],
    mu_pre = mu_diffuse_filter[i],
    P_pre = P_diffuse_filter[i],
    sigma_w = sigma_w,
    sigma_v = sigma_v
  )
  mu_diffuse_filter[i + 1] <- kekka$mu_filter
  P_diffuse_filter[i + 1] <- kekka$P_filter
  y_resid_diffuse[i] <- kekka$y_resid
  F_diffuse[i] <- kekka$F
  K_diffuse[i] <- kekka$K
}


data_vis <- data.frame(month = monthly_data$month,
                       usj = monthly_data$USJ,
                       filter = mu_filter[-1],
                       smooth = mu_smooth[-1],
                       diffuse_filter = mu_diffuse_filter[-1]
                      )
data_vis <- melt(data_vis, id.vars = "month")

ggplot(data = data_vis,
       aes(x = month, y=value, colour=variable)) + geom_line()

#dnorm関数を使った対数尤度の計算
sum(
  log(
    dnorm(y_resid_diffuse[-1],
          mean = 0,
          sd = sqrt(F_diffuse[-1]))
  )
)

#対数尤度の計算
-1 * ((N -1)/2) * log(2 * pi) - 1/2 * sum(log(F_diffuse[-1]) + y_resid_diffuse[-1]^2 / F_diffuse[-1])


##dlmを用いたカルマンフィルター
#dlmのパラメータの設定
mod_dlm <- dlmModPoly(
  order = 1,
  m0 = 0,
  C0 = 10000000,
  dW = sigma_w,
  dV = sigma_v
)

#カルマンフィルタの実行
mu_filter_dlm <- dlmFilter(ts_monthly_data, mod_dlm)

#作成した関数とdlmの推定結果の違い
sum((mu_filter_dlm$m[-1] - mu_filter[-1])^2)

#対数尤度の指標
dlmLL(ts_monthly_data, mod_dlm)

#比較
1/2 * sum(log(F) + y_resid^2 / F)

mu_smooth_dlm <- dlmSmooth(mu_filter_dlm)

sum((mu_smooth_dlm$s - mu_smooth)^2)


##dlmの使い方
#STEP1:モデルの構造
build_local_level_dlm <- function(theta){
  dlmModPoly(order = 1,
             dV = exp(theta[1]),
             dW = exp(theta[2]))
}

#STEP2:パラメータ推定
par_local_level_dlm <- dlmMLE(ts_monthly_data,
                              parm = c(1, 1),
                              build_local_level_dlm)
推定された分散を使ってモデルを組み直す
fit_local_level_dlm <- build_local_level_dlm(par_local_level_dlm$par)

#STEP3:フィルタリング
filter_local_level_dlm <- dlmFilter(ts_monthly_data, fit_local_level_dlm)

#STEP4:スムージング
smooth_local_level_dlm <- dlmSmooth(filter_local_level_dlm)

#最尤法によるパラメータ推定
exp(par_local_level_dlm$par)

#フィルタ化推定量
autoplot(filter_local_level_dlm,
         fitted.colour = "black",
         fitted.size = 1.5,
         main = "フィルタ化推定量")
#平滑化状態の図示
p_usj <- autoplot(ts_monthly_data)
autoplot(smooth_local_level_dlm,
         fitted.colour = "black",
         colour = "black",
         size = 1.5,
         main = "平滑化状態",
         p = p_usj)


#KFASによる散漫カルマンフィルタ
mod_kfas <- SSModel(
  H = sigma_v,
  ts_monthly_data ~ SSMtrend(degree = 1, Q = sigma_w)
)

#散漫カルマンフィルタの実行
mu_filter_kfas <- KFS(mod_kfas,
                      filtering = c("state", "mean"),
                      smoothing = "none")

#関数とKFASの結果の比較
sum((mu_filter_kfas$a - mu_diffuse_filter)^2)

#KFASによる散漫対数尤度の計算
logLik(mod_kfas)


