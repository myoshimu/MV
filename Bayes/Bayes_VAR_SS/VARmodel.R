install.packages("vars")
library(vars)

##他のデータ(G7の株式収益率)
msci <- read.table("msci.txt", header=T)
msci <- msci[,-1]
head(msci)

#日本とカナダだけにしてみる
ca.jp <- msci[,c(1,5)]
head(ca.jp)
plot(ca.jp$ca, type="l")  #定常性全然だめ

#対数階差収益にする。
log(ca.jp)
ca.er <- 100*diff(log(ca.jp)$ca,1)  #earning rate
jp.er <- 100*diff(log(ca.jp)$jp,1)
ca.jp.er <- data.frame(ca.er, jp.er)
head(ca.jp.er)
plot(ca.jp.er$ca.er, type="l") #定常っぽい

#簡単にするため今回ラグは2くらいにしておく
var.ca.jp <- VAR(ca.jp.er, p=2, type="const")
summary(var.ca.jp) #解釈してみる(分散共分散行列が大切かも)

#予測1
pred1 <- predict(var.ca.jp, n.ahead=10, ci=0.95)
pred1

#最適なラグはいくつ？
var.ca.jp.aic <- VAR(ca.jp.er, lag.max=5, ic="AIC", type="const")
summary(var.ca.jp.aic) #ラグは3が選ばれてる

#予測2
pred2 <- predict(var.ca.jp.aic, n.ahead=10, ci=0.95)
pred2

#インパルス応答関数　#沖本2010参照#
head(msci)

#VARを7カ国で組んでみる
#ラグのセレクト　こんな関数もある
VARselect(msci, type="const")
#AIC的には3が良いらしいので
var.msci <- VAR(msci, type="const", p=3)
summary(var.msci)

#直交化インパルス関数 
#epsilonに1単位の変動(増加)を与えた時の反応
#Rでは分散共分散行列のコレスキー分解してるらしい
msci.irf <- irf(var.msci, n.ahead=10, ci=0.95)
msci.irf
plot(msci.irf)

#グレンジャー因果の検定
causality(var.msci, cause="us")
causality(var.msci, cause="jp")
