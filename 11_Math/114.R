d3 <- read.csv("Personality1.csv", fileEncoding="cp932")
head(d3)
str(d3)

#ダミー変数を使った線形判別
res <-lda(血液型~.,data=d3[-41,])
res

#自分の血液型は当たるか
predict(res, d3[41,])

