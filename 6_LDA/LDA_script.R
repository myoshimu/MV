#データの読み込み(最後のnpを予測できるか?)
d <- read.csv("media2.csv", fileEncoding="cp932")
d

#線形判別分析
library(MASS)
lda1 <- lda(メディア ~ ., data=d[-20,])
lda1

#トレーニングデータの正誤表
predict(lda1)
t <- table(predict(lda1)$class, d[-20,]$メディア)
t

#テストデータの予測
predict(lda1, d[20,-1])

####################
#irisデータの例示
iris
iris[c(1,51,101),]

#setosa種とvirginica種を取り出す
seto <- subset(iris[1:50,],select=-Species)
virgi <- subset(iris[101:150,],select=-Species)

#2種の平均を求める(どこがトレーニングでどこがテストか考えてみよう)
seto.m <- apply(seto[1:45,],2,mean)
virgi.m <- apply(virgi[1:45,],2,mean)
seto.m
virgi.m

#2種の分散共分散行列を求める
seto.v <- var(seto[1:45,])
virgi.v <- var(virgi[1:45,],)
seto.v
virgi.v

#マハラノビス汎距離を計算
D1 <- mahalanobis(seto[46:50,],seto.m,seto.v)
D2 <- mahalanobis(seto[46:50,],virgi.m,virgi.v)
cbind(D1,D2)

################





