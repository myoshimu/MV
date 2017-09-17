#文章の各品詞出現頻度から文書の種別を予測
d<-read.csv("media2.csv", fileEncoding = "cp932")
d

#線形判別分析
library(MASS)
#19行目までを教師データとして(one leave out)lda関数で判別
lda1 <- lda(メディア~., data=d[-20,])
lda1
#Group meansの項目で各メディアの平均出現数をみる


#トレーニングデータの正誤表
predict(lda1)
#posteriorは各クラスに属する事後確率、$xが判別得点
t<-table(predict(lda1)$class, d[-20,]$メディア)
t

#20行目でテストデータ予測
predict(lda1, d[20,-1])
