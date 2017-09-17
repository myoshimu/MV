#データの確認
d <- read.csv("Ramen.csv", fileEncoding = "cp932")
d

#標準化(後で使う)
d.scale <- scale(d[,-1])
d.scale

#相関行列
cor.mat <- cor(d[,-1])
cor.mat

#固有値・固有ベクトル
eg <- eigen(cor.mat)
eg

#どういう意味？ roundは近い整数にまるめる関数
round(sum(eg$vectors[,1]*eg$vectors[,2]))
round(sum(eg$vectors[,2]*eg$vectors[,3]))
round(sum(eg$vectors[,2]*eg$vectors[,1]))

#固有ベクトルの値から主成分得点の式を計算
z1 <- 0.57*d.scale[,1] + 0.52*d.scale[,2] + 0.63*d.scale[,3]
z2 <- 0.60*d.scale[,1] - 0.79*d.scale[,2] + 0.11*d.scale[,3]
z1 ; z2

#ラベル付き散布図を作るのに楽
install.packages("maptools")
library(maptools)

plot(z1,z2, type="n")
pointLabel(z1, z2, labels=d[,1])

#第二主成分までの寄与率
cumsum(eg$values)/3

#prcomp関数による主成分分析
d <- read.csv("Ramen.csv", fileEncoding="cp932")
d
rownames(d) <- d$店名 #行名の書き換え(biplotがみやすい)
d

#prcomp関数の利用
pr <- prcomp(d[,-1], scale=TRUE)
pr

#主成分得点
pr$x

#主成分負荷量(元の変数と主成分の相関)
t(t(pr$rotation)*pr$sdev)

#累積寄与率
cumsum(pr$sdev^2)/3

#バイプロット
biplot(pr) #上下逆転?(方向と向き)


##########mtcarsの分析#########
mtcars #R内のデータ
?mtcars #help開く

#prcompの利用
p.mt <- prcomp(mtcars, scale=TRUE)
p.mt

#累積寄与率
cumsum(p.mt$sdev^2)/11

#バイプロット
biplot(p.mt, cex=0.7) #cexは文字サイズ指定
