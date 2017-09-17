#データの読み込みと標準化
TOEIC <- read.csv("ch13cluster.csv")
TOEIC.s <- scale(TOEIC[,-1])

#距離を計算(ユークリッド)
dis <- dist(TOEIC.s)
dis

#クラスター分析(ウォード法)とデンドログラムの表示
hcr <- hclust(dis, method="ward.D2")
plot(hcr)

#クラスターをカット
cluster <- cutree(hcr, k=3)
table(cluster)

#元のデータフレームと統合
result <- data.frame(TOEIC, cluster)
result

#クラスター平均を求めて、各クラスターを解釈
aggregate(.~cluster, result, mean)