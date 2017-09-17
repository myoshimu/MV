#階層的クラスタ
#データの読み込みと標準化
#Intrinsic_motivationは点数向上の内的要因
TOEIC <- read.csv("ch13cluster.csv")
TOEIC.s <- scale(TOEIC[,-1])

#距離を計算（ユークリッド）
#()で区切った式は代入とプリントを両方やってくれる
(dis <- dist(TOEIC.s))

#クラスタ分析（ウォード法）とデンドログラム表示
hcr <- hclust(dis, method="ward.D2")
plot(hcr)

#クラスタをカットし元のデータフレームと統合
cluster <- cutree(hcr, k=3)
table(cluster)
(result <- data.frame(TOEIC, cluster))

#クラスタ平均を求めて、各クラスタを解釈
aggregate(.~cluster, result, mean)
#1はOral_RehearsalやAssociationが高く話せるがモチベーションが低い
#2は点数もモチベーションも高く優秀な人
#3は話せないけどモチベーションが高い