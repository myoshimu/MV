#ラーメン屋の評価を主成分分析（麺・具・スープ）
#データの確認
d<-read.csv("Ramen.csv", fileEncoding = "cp932")
d

#標準化
d.scale <- scale(d[,-1])
d.scale

#相関行列
cor.mat <- cor(d[,-1])
cor.mat

#固有値・固有ベクトル
eg <- eigen(cor.mat)
eg

round(sum(eg$vectors[,1]*eg$vectors[,2]))
round(sum(eg$vectors[,2]*eg$vectors[,3]))
round(sum(eg$vectors[,2]*eg$vectors[,1]))

#固有ベクトルの値から主成分得点の式を計算
z1<-0.57*d.scale[,1] + 0.52*d.scale[,2] + 0.63*d.scale[,3]
z2<-0.60*d.scale[,1] - 0.79*d.scale[,2] + 0.11*d.scale[,3]
z1 ; z2

#maptoolsでbiplot作成（ラベル付き散布図を作るのに楽）
install.packages("maptools")
library(maptools)
plot(z1,z2,type="n")
text(z1,z2)

#固有値の和から累積寄与率を求める
cumsum(eg$values)/3
eg$values
