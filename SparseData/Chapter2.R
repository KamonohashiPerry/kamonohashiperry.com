library(glmnet)
library(ncvreg)
set.seed(0)

#人工データの生成
z1 <- runif(100, 0, 20)
z2 <- runif(100, 0, 20)
x <- scale(cbind(z1 + rnorm(100, 0, 0.1),
                 -z1 + rnorm(100, 0, 0.1),
                 z1 + rnorm(100, 0, 0.1),
                 z2 + rnorm(100, 0, 0.1),
                 -z2 + rnorm(100, 0, 0.1),
                 z2 + rnorm(100, 0, 0.1)))

#データの確認
pairs(x)


y <- 3*apply(x[ , 1:3], 1, sum) + 
      apply(x[ , 4:6], 1, sum) + rnorm(100)

#ヒストグラムの確認
hist(y)

#エラスティックネット
elasticnet_fit <- glmnet(x, y, alpha = 0.5)

#エラスティックネットの解パス図の描画
plot(elasticnet_fit, 
     xvar = "lambda",
     label = TRUE,
     xlab = "正則化パラメータの対数値",
     ylab = "回帰係数",
     col = "black",
     lwd = 2.5)

#lasso
lasso_fit <- glmnet(x, y)

#lassoの解パス図の描画
plot(lasso_fit,
     xvar = "lambda",
     label = TRUE,
     xlab = "正則化パラメータの対数値",
     ylab = "回帰係数",
     col = "black",
     lwd = 2.5)

Data <- read.table("crime.txt")
x <- scale(as.matrix(Data[ , -c(1, 2)]))
y <- Data[ , 1]

#SCAD
opt_lambda <- cv.ncvreg(x, y, penalty = "SCAD")$lambda.min
ncvreg(x, y, penalty = "SCAD", lambda = opt_lambda)$beta

#MC+
opt_lambda <- cv.ncvreg(x, y, penalty = "MCP")$lambda.min
ncvreg(x, y, penalty = "MCP", lambda = opt_lambda)$beta

#適応型lasso
W <- diag(1/abs(coef(lm(y~x))[-1]))
xstar <- x %*% solve(W)
opt_lambda <- cv.glmnet(xstar, y, standardize = FALSE)$lambda.min
adalasso_fit <- glmnet(xstar, y, lambda = opt_lambda,
                       standardize = FALSE)

solve(W) %*% adalasso_fit$beta

