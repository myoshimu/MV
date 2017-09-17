d2 <- read.csv("Ryokan.csv", fileEncoding="cp932")
d2

#数量二類（ダミー変数を使った線形判別分析）
library(MASS)
Q2 <- lda(再利用~., data=d2[-10,-1])
Q2

predict(Q2, d2[10,-1]) #10番目のデータだけでホールドアウト
