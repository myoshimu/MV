#ネットワーク分析
library(igraph)

#無向グラフの隣接行列
(test<-matrix(c(0,1,1,1, 1,0,0,1, 1,0,0,0, 1,1,0,0),ncol=4))
colnames(test)<-c("A","B","C","D")
rownames(test)<-c("A","B","C","D")
test

test.g <- graph.adjacency(test,weight=T)
plot(test.g)

#有向グラフの隣接行列
(test<-matrix(c(0,0,1,0, 1,0,0,0, 2,0,0,0, 4,3,0,0),ncol=4))
colnames(test)<-c("A","B","C","D")
rownames(test)<-c("A","B","C","D")
test

test.g <- graph.adjacency(test,weight=T)
plot(test.g)

#グラフを装飾
plot(test.g, layout=layout.circle, edge.label=E(test.g)$weight)

#好感度データの作成
n1 <- c("Seshita", "Uchiba", "Uchiba", "Umezaki", "Sato", "Shirakawa", "Shirakawa")
n2 <- c("Uchiba", "Seshita", "Umezaki", "Watanabe", "Watanabe", "Seshita", "Sato")
fav <- c(10,2,9,6,8,8,5)
test2 <- data.frame(n1,n2,fav)
test2

test2.g <- graph.data.frame(test2)
test2.g
plot(test2.g, edge.label=test2[,3])
