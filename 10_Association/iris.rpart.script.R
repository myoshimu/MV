#パッケージのインストール
install.packages("rpart")
install.packages("partykit")
library(rpart)
library(partykit)

#有名なirisデータ
iris

#テストとトレーニングに
index <- sample(1:150, 110)
train <- iris[index,]
test <- iris[-index,]

#決定木
(dt <- rpart(Species ~. ,data=train))

#決定木の表示
plot(as.party(dt))

#予測
predict(dt, test)
res <- predict(dt, test, type="class")
(t <- table(res, test$Species))
(cr <- sum(diag(t))/sum(t))






