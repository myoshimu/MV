#並列化演算の設定（コア数2の場合）
library(doParallel)
c1<-makePSOCKcluster(2)
registerDoParallel(c1)

#データの用意（スパムメールに含まれてる特殊文字の数）58変数
library(kernlab)
data(spam) #データ呼び出し
str(spam)
index <- sample(1:4601,2500)
train <- spam[index,]
test <- spam[-index,]

#bagging(bootstrap aggregating)、デフォルト学習機は100個, ?bagging
#各学習機は独立、並行化が容易
library(adabag)
(spam.ba <- bagging(type~.,data=train))
spam.ba.pre <- predict(spam.ba, test)
spam.ba.pre$confusion
1-spam.ba.pre$error #正答率
spam.ba$trees #各学習機の分類詳細

#boosting
#間違えたものをより敏感に感知するよう重み付けするので精度高い
#ただし並行化は難しい
spam.bst <- boosting(type~., data=train)
spam.bst.pre <- predict(spam.bst, test)
spam.bst.pre$confusion
1-spam.bst.pre$error

#randomForest(mtryはsqrt(変数数)で決めてる)
library(randomForest)
spam.rf <- randomForest(type~., data=train, importance=T)
spam.rf.pre <- predict(spam.rf,test)
t.rf <- table(test$type, spam.rf.pre)
(c.rf <- sum(diag(t.rf))/sum(t.rf))
par(mfrow=c(1,1))
plot(spam.rf) #classエラーと全体エラー


#変数の重要性（ここは大切！）
importance(spam.rf)
varImpPlot(spam.rf)
#左がOOBエラー減らす変数で右がジニ係数を減らす＝決定木の上の方にある変数
#両方で上にある変数が特に重要と考えられる

#決定木に使う変数数をチューニングしてみる
tuneRF(spam[,-58],spam[,58], doBest=T)
(spam.rf.tune <- randomForest(type~., data=train,mtry=7))


#結果としてやっぱり変数数は7でOKということになる
#なので通常mtryは変数数のルートでよい
(t.rf.tune <- table(test$type, predict(spam.rf.tune, test)))
(cr.rf.tune <- sum(diag(t.rf.tune))/sum(t.rf.tune))