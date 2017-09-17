library(randomForest)

test <- read.csv("wine_white_test.csv")
dat <- read.csv("wine_white_train.csv")

#データ型を確認
str(dat)

#qualityがintとして認識されてしまっていてこのままではうまくいかないのでfactorに変換
#一度文字にしてからfactorにするのが扱いやすくておすすめ
test[,12] <- as.factor(as.character(test[,12]))
dat[,12] <- as.factor(as.character(dat[,12]))

#randomForest(mtryはsqrt(変数数)で決めてる)
rf <- randomForest(quality~., data=dat, importance=T)
rf.pre <- predict(rf,test)
t.rf <- table(as.character(test$quality), rf.pre)
(c.rf <- sum(diag(t.rf))/sum(t.rf))
par(mfrow=c(1,1))
plot(rf) #classエラーと全体エラー


#変数の重要性（ここは大切！）
importance(rf)
varImpPlot(rf)
#左がOOBエラー減らす変数で右がジニ係数を減らす＝決定木の上の方にある変数
#両方で上にある変数が特に重要と考えられる


library(rpart) #決定木のパッケージ
library(partykit)
#決定木
(dt <- rpart(quality~.,data=dat))

#決定木の表示（indexからplotまで繰り返すと結果がたまに変わる
#決定木は再現性に乏しいため（データロバストでない）
#でもアンサンブル学習ではこれが良い方向に働く
plot(as.party(dt))

#予測
predict(dt, test)
res <- predict(dt, test, type="class")
(t <- table(res,test$quality))
(cr <- sum(diag(t)/sum(t))) #正答率