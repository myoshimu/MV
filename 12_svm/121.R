install.packages("doParallel")
library(doParallel)
detectCores() #コア数の表示
c1 <- makePSOCKcluster(4)
registerDoParallel(c1)

#ライブラリとデータ
library(kernlab)
data(spam)
str(spam)

#トレーニングとテストに分類
index <- sample(1:4601, 3000)
train <- spam[index,]
test <- spam[-index,]

#線形SVM(vanilladotがカーネル)
l.svm <- ksvm(type~., data=train, kernel="vanilladot")
l.pre <- predict(l.svm, test)
(l.t <- table(l.pre, test$type))
(l.cr <- sum(diag(l.t))/sum(l.t))




#spamをカーネルSVMで
k.svm <- ksvm(type~., data=train, kernel="rbfdot")
k.pre <- predict(k.svm, test)
(k.t<-table(k.pre,test$type))
(k.cr<-sum(diag(k.t)/sum(k.t)))

#くり返し処理用
l.cr <- NULL
k.cr <- NULL

for(i in 1:50){
  index <-sample(1:4601, 3000)
  train <- spam[index,]
  test <- spam[-index,]
  
  l.svm <- ksvm(type~., data=train, kernel="vanilladot")
  l.pre <- predict(l.svm, test)
  l.t <- table(l.pre, test$type)
  l.cr[i] <- sum(diag(l.t))/sum(l.t)
  
  k.svm <- ksvm(type~., data=train, kernel="rbfdot")
  k.pre <- predict(k.svm, test)
  k.t <- table(k.pre, test$type)
  k.cr[i] <- sum(diag(k.t))/sum(k.t)
}

mean(l.cr)
mean(k.cr)

#ランダムフォレストで重要度を計算して重要そうな変数を確認
library(randomForest)
rf <- randomForest(type~., data=spam, importance=T)
varImpPlot(rf)

#重要そうな変数だけでやってみる
l.cr <- NULL
k.cr <- NULL
for(i in 1:50){
  index <- sample(1:4601, 3000)
  train <- spam[index,]
  test <- spam[-index,]
  l.svm <-ksvm(type~charExclamation + capitalAve + hp + remove + charDollar + free + your, data=train,kernel="vanilladot")
  l.pre <- predict(l.svm, test)
  l.t <- table(l.pre, test$type)
  l.cr[i] <- sum(diag(l.t))/sum(l.t)
  
  k.svm <- ksvm(type~charExclamation + capitalAve + hp + remove + charDollar + free + your, data=train, kernel="rbfdot")
  k.pre <- predict(k.svm,test)
  k.t <- table(k.pre, test$type)
  k.cr[i] <- sum(diag(k.t))/sum(k.t)
}

#変数を絞ると違いが出る
mean(l.cr)
mean(k.cr)