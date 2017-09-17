#データの読み込み
d <- read.csv("shinsotsu.csv", fileEncoding="cp932")
head(d)
str(d)

#相関のチェック
cor(d[,-1])

#####################

#Curryデータの読み込み
d2 <- read.csv("Curry.csv", fileEncoding="cp932")
head(d2)
str(d2)

#標準化
d2.s <- scale(d2[,-1])

#相関行列を確認
cor(d2.s)

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

#詳細
print(fa2, sort=T)


##############

#shinsotsuデータの読み込み
d <- read.csv("shinsotsu.csv", fileEncoding="cp932")
head(d)
str(d)

#3因子モデル　プロマックス回転・バートレット法で因子得点
fa.3 <- factanal(d[,-1], factors=3,
                rotation="promax", scores="Bartlett")
print(fa3, sort=T)

#寄与率と独自性の関係
mean(fa.3$uniquenesses)+0.843 #0.843は累積寄与率
round(mean(fa.3$uniquenesses)+0.843)

#因子得点の確認
fa.3$scores

#2因子との比較
fa.2 <- factanal(d[,-1], factors=2,
                 rotation="promax", scores="Bartlett")
print(fa.2, sort=T)
