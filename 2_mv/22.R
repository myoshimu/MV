#問1 データの散布図
d <- read.csv("Multi-lm.csv")
plot(d$X,d$Y)

#問2 1時の線形回帰式を適用し、自由度調整済み寄与率を出せ。
ML1 <- lm(Y~X, data=d)
summary(ML1)

#問3 3次、5次の回帰式を適用
ML3 <- lm(Y~X+I(X^2)+I(X^3), data=d)
summary(ML3)
ML5 <- lm(Y~X+I(X^2)+I(X^3)+I(X^4)+I(X^5), data=d)
summary(ML5)

#9次だと過適合でNoneになってしまう
ML9 <- lm(Y~X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6)+I(X^7)+I(X^8)+I(X^9), data=d)
summary(ML9)

#問4 散布図であてはまりチェック
plot(d$X, d$Y, pch=19, cex=1.5) #オプションは文字の拡大率
X1 <- seq(0, 10, by=0.05) # 0から10まで0.05刻みの変数作成
new.data <- data.frame(X=X1) #データ置き換え
Y1 <- predict(ML1, new.data) # 線形回帰からの予測値
Y3 <- predict(ML3, new.data) # 3次多項式回帰からえられる予測値
Y5 <- predict(ML5, new.data)
Y9 <- predict(ML9, new.data)
matlines(X1, cbind(Y1,Y3,Y5,Y9), lty=4:1, col=4:1) #3本の回帰式を描く
par(family="Osaka")
legend("bottomright", c("1次式","3次式","5次式","9次式"), lty=4:1)


#問6 累乗モデルによる回帰
pressure2 <- pressure[-1,]

#散布図の確認
par(mfrow=c(1,2))
plot(pressure ~ temperature, pressure2, pch=19)
plot(log(pressure) ~ log(temperature), pressure2, pch=19)

#古典的モデル
ans1 <- lm(log(pressure) ~ log(temperature), pressure2)
abline(ans1)
summary(ans1)

#係数の算出
a1 <- exp(coefficients(ans1)[1])
b1 <- coefficients(ans1)[2]

#非線形最小二乗法
ans2 <- nls(pressure~a*temperature^b, pressure2,
            start=list(a=a1,b=b1),  #初期値
            control=nls.control(maxiter=70))
summary(ans2)

plot(pressure~temperature, data=pressure2, pch=19)
#nlsのライン
lines(pressure2$temperature, predict(ans2, newdata=pressure2),col=2)
#古典のライン
lines(pressure2$temperature, exp(predict(ans1, newdata=pressure2)),col=1)





#問9 指数モデル
plot(log(pressure)~temperature, pressure2, pch=19)
ans3 <- lm(log(pressure)~temperature, pressure2)
abline(ans3)
summary(ans3)

#係数の算出
a2 <- exp(coefficients(ans3)[1])
b2 <- exp(coefficients(ans3)[2])

#非線形最小二乗法
ans4 <- nls(pressure ~ a*b^temperature, pressure2,
            start=list(a=a2,b=b2))
summary(ans4)

# 理論式(圧力と気温の関係の公式から)
ans5 <- nls(pressure ~ exp(a/(temperature+273)+b), pressure2,
            start=list(a=1,b=1))
summary(ans5)

plot(pressure~temperature, data=pressure2, pch=19)
lines(pressure2$temperature, exp(predict(ans3, newdata=pressure2)),col=1)
lines(pressure2$temperature, predict(ans4, newdata=pressure2),col=2)
lines(pressure2$temperature, predict(ans5, newdata=pressure2),col=3)


