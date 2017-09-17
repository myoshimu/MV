library(rpart) #決定木のパッケージ
library(partykit)

#irisをテストとトレーニングに分ける
index <- sample(1:150, 110) #乱数で分ける
train <- iris[index,]
test <- iris[-index,]

#決定木
(dt <- rpart(Species~.,data=train))

#決定木の表示（indexからplotまで繰り返すと結果がたまに変わる
#決定木は再現性に乏しいため（データロバストでない）
#でもアンサンブル学習ではこれが良い方向に働く
plot(as.party(dt))

#予測
predict(dt, test)
res <- predict(dt, test, type="class")
(t <- table(res,test$Species))
(cr <- sum(diag(t)/sum(t))) #正答率