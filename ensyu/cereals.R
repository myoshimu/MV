#主成分分析
#prcomp実行
pr<-prcomp(d[,-1:-3],scale=TRUE)
pr

#主成分得点
pr$x

#主成分負荷量
t(t(pr$rotation)*pr$sdev)

#累積寄与率
cumsum(pr$sdev^2)/3

#バイプロット
par(family="HiraginoSans-W3")
biplot(pr,type="n")

rownames(pr) <- d[,2]






#階層的クラスタ
d.s <- scale(d[,-1:-3])

#距離を計算（ユークリッド）
#()で区切った式は代入とプリントを両方やってくれる
(dis <- dist(d.s))

#クラスタ分析（ウォード法）とデンドログラム表示
#2つで分けるのがよさそう
hcr <- hclust(dis, method="ward.D2")
plot(hcr)

#クラスタをカットし元のデータフレームと統合
cluster <- cutree(hcr, k=2)
table(cluster)
(result <- data.frame(df, cluster))

#クラスタ平均を求めて、各クラスタを解釈
aggregate(.~cluster, result, mean)

#クラスタ平均の結果から、
#1はcalories,protein,fat,fiber,potassが高くボリューム重視
#2はfatが低く、fiber, sugars,potassが低く健康思考が高い製品群と考えられる
#また、主成分分析のバイプロットからもこの傾向は読み取れ1グループに属するのは1,3,4
#2に属するものは40,47,71などがあげられる

#あとはブランド名をマッピングするといい


