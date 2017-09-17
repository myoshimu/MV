#prcompによる主成分分析
d<-read.csv("Ramen.csv",fileEncoding = "cp932")
d
#行名の書き換え（biplotで見やすいように）
rownames(d)<-d$店名
d
#prcomp実行
pr<-prcomp(d[,-1],scale=TRUE)
pr

#主成分得点
pr$x

#主成分負荷量
t(t(pr$rotation)*pr$sdev)

#累積寄与率
cumsum(pr$sdev^2)/3

#バイプロット
par(family="HiraginoSans-W3")
biplot(pr)