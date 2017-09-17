#データの読み込みと確認
d <- read.table("SwissBank.txt", header=T)
head(d)
str (d)
summary(d)

#クラスごとの平均
aggregate(.~class, data=d, mean)

#トレーニングとテストの分割(ホールドアウト)
train <- d[c(1:80, 101:180),]
test <- d[c(81:100, 181:200),]

#線形判別分析
library(MASS)
LDA <- lda(class~ . ,data=train)

#再代入正答率
rep <- predict(LDA)$class
(t1 <- table(rep, train$class))
(cr1 <- sum(diag(t1))/sum(t1))

#予測正答率
p <- predict(LDA, test)$class
(t2 <- table(p, test$class))
(cr2 <- sum(diag(t2))/sum(t2))
