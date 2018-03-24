library(glmnet)

crime <- read.table("crime.txt")
crime <- as.matrix(crime)

x <- crime[, 3:7]
y <- crime[, 1]
x <- scale(x)
y <- y -mean(y)

# lasso推定
res <- glmnet(x, y)

#解パス図描画
plot(res, xvar = "lambda", label = TRUE, xlab = "正則化パラメータの対数値",
     ylab = "回帰係数", col = "black", lwd = 2.5)

res1 <- glmnet(x, y, lambda = 20)
res1$beta # 係数の推定値
res.cv <- cv.glmnet(x, y)

#CV値の推移をプロット
plot(res.cv, xlab = "正則化パラメータの対数値", ylab = "誤差2乗和")

#CV値が最小となる正則化パラメータの値を出力
res.cv$lambda.1se

