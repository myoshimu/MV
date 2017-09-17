iris

#種別を省いて一回だけkmeans
iris.km <- kmeans(iris[,-5],centers=3, iter.max=1)
iris.km

#可視化してみる
library(cluster)
plot(pam(iris[,1:4],3),ask=TRUE) #主成分軸2つで2を選択
#コンソールで0を選択すると抜ける

#kmeansの弱点:初期値によって変な分類になってしまう
d <- read.csv("kmeans.demo.csv")
df <- d[,-1]

#下の3行を何回か実行すると何回目かで自然な分類になる
clust <- kmeans(df, centers=3, iter.max=1)$cluster
result <- data.frame(df, clust)
plot(df$x1, df$y1, col=clust, pch=clust)

#iterを増やすとデフォルトで10回繰り返して多数決をとってくれる
clust <- kmeans(df, centers=3, iter.max=10)$cluster
result <- data.frame(df,clust)
plot(df$x1, df$y1, col=clust, pch=clust)

#おまけ(EMアルゴリズムによる混合分布を用いたクラスタ推定)
library(mclust)
irisBIC <- mclustBIC(iris[,-5],)
irisBIC
plot(irisBIC)
#2変数のVEV楕円形同体積がBICが大きいので一番良いモデル

#モデルごとのBICを計算
irisBIC1 <- mclustBIC(iris[,-5], G=seq(from=1, to=9, by=1), modelNames=c("EII","EEI","EEE"))
irisBIC1
plot(irisBIC1)

