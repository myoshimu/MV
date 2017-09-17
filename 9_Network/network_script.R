#igraph packageの用意
install.packages("igraph")
library(igraph)

#無向グラフの隣接行列
(test <- matrix(c(0,1,1,1, 1,0,0,1, 1,0,0,0, 1,1,0,0), ncol=4))
colnames(test) <- c("A", "B", "C", "D")
rownames(test) <- c("A", "B", "C", "D")
test

test.g <- graph.adjacency(test, weight=T)
plot(test.g)

#有向グラフの隣接行列
(test <- matrix(c(0,0,1,0, 1,0,0,0, 2,0,0,0, 4,3,0,0), ncol=4))
colnames(test) <- c("A", "B", "C", "D")
rownames(test) <- c("A", "B", "C", "D")
test

test.g <- graph.adjacency(test, weight=T)
plot(test.g)

#グラフを装飾
plot(test.g, layout=layout.circle, edge.label=E(test.g)$weight)

#好感度データの作成
n1 <- c("Seshita", "Uchiba", "Uchiba", "Umezaki", "Sato" ,"Shirakawa", "Shirakawa")
n2 <- c("Uchiba", "Seshita", "Umezaki", "Watanabe", "Watanabe", "Seshita", "Sato")
fav <- c(10,2,9,6,8,8,5)
test2 <- data.frame(n1,n2,fav)
test2

test2.g <- graph.data.frame(test2)
test2.g
plot(test2.g, edge.label=test2[,3])


#実際の例(レミゼラブル　valuesは同じ章の同じ場面に現れた回数)
g <- read.graph("lesmis.gml", format="gml")
g
plot(g,weights=E(g)$values)

#(媒介)中心性 中心性には色々ある　固有ベクトル中心性とか
#あるノードに対して、その他の2点を結ぶ最短経路がどれくらいそのノードを通過しているか
g_bw<-betweenness(g,directed = F,weights = E(g)$value)
g_bw

#スピングラス法によるコミニュティ分析(サブグラフ検出)
#クラスター化と言ってもいい。クラスター度合いはQ値で測る
#Q値クラスタ内の辺密度が高く、クラスタ間の辺密度が低いと、高いQ値!(アイデアは何かと似てるね)
#スピングラス法(別名焼きなまし法)
# Q_0 < Q_1なら採択
# Q_0 > Q_1なら確率exp(\beta|Q_1 - Q_0|)で採択(かつbetaを徐々に大きくする = 温度を低下させる)

g_com<-spinglass.community(g,weights = E(g)$value,spins = 50)
g_com

plot(g,vertex.size=log(50*g_bw+100),
     vertex.color=g_com$membership,
     layout=layout.fruchterman.reingold,
     margin=-0.2)
