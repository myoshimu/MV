library(vars)

##他のデータ（G7の株式収益率）
msci <- read.table("msci.txt", header=T)
msci <- msci[,-1]
head(msci)

#日本とカナダだけにする
ca.jp <- msci[,c(1,5)]
head(ca.jp)
plot(ca.jp$ca, type="l") #定常生はなさそう

log(ca.jp)
ca.er <- 100*diff(log(ca.jp)$ca,1) #earning rate
jp.er <- 100*diff(log(ca.jp)$jp,1)
ca.jp.er <- data.frame(ca.er, jp.er)
head(ca.jp.er)
plot(ca.jp.er$ca.er, type="l")

var.ca.jp <- VAR(ca.jp.er, p=2, type="const")
summary(var.ca.jp)

#予測1
pred1 <- predict(var.ca.jp, n.ahead=10, ci=0.95)
pred1

#最適なラグは？
var.ca.jp.aic <- VAR(ca.jp.er, lag.max=5, ic="AIC", type="const")
summary(var.ca.jp.aic) #ラグは3が選ばれる


#予測2
pred2 <- predict(var.ca.jp.aic, n.ahead=10, ci=0.95)
pred2

#VARを7国で組んでみる
VARselect(msci, type="const")
var.msci <- VAR(msci, type="const", p=3)
summary(var.msci)

msci.irf <- irf(var.msci, n.ahead=10, ci=0.95)
msci.irf
plot(msci.irf)

