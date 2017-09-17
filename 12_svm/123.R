#手法の比較
data(spam)
#トレーニングとテストに分類
index <- sample(1:4601, 3000)
train <- spam[index,]
test <- spam[-index,]


#決定木(0.87)
library(rpart) #決定木のパッケージ
library(partykit)
(dt <- rpart(type~.,data=train))
plot(as.party(dt))
predict(dt, test)
res <- predict(dt, test, type="class")
(t <- table(res,test$type))
(cr <- sum(diag(t)/sum(t))) #正答率


#ランダムフォレスト(0.95)
library(randomForest)
spam.rf <- randomForest(type~., data=train, importance=T)
spam.rf.pre <- predict(spam.rf,test)
t.rf <- table(test$type, spam.rf.pre)
cr.rf <- sum(diag(t.rf))/sum(t.rf)
cr.rf
plot(spam.rf)
importance(spam.rf)
varImpPlot(spam.rf)

