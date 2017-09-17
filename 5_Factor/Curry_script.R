#データ読み込み
d <- read.csv("Curry.csv", fileEncoding="cp932")
head(d)
str(d)

#相関行列のチェック
cor(d[,-1])

#factanal関数による因子分析2因子(無回転)
fa2 <- factanal(d[,-1], factors=2, rotation="none")
print(fa2$loadings, sort=T) #因子負荷量が大きい順に並べ替え

#因子負荷量をplotしてみる(無回転)
plot(fa2$loadings[,1],fa2$loadings[,2],
     xlim=c(0,1), ylim=c(-1,1))
abline(a=0, b=0, lty=2, col=2) #この2行はx軸, y軸を書いているだけ
abline(v=0, lty=2, col=2)

#factanal関数による因子分析(varimax回転)
fa2.vari <- factanal(d[,-1], factors=2, rotation="varimax")
print(fa2.vari$loadings, sort=T)

#因子負荷量をplotしてみる(回転後)
plot(fa2.vari$loadings[,1],fa2.vari$loadings[,2],
     xlim=c(0,1), ylim=c(-1,1))
abline(a=0, b=0, lty=2, col=2)
abline(v=0, lty=2, col=2)


