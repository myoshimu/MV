# 並列化を行う
install.packages("doParallel")
library(doParallel)
detectCores() #コア数の表示
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

#ライブラリとデータ
library(kernlab)
data(spam)
str(spam)

#トレーニングとテストに分割
index <- sample(1:4601, 3000)
train <- spam[index,]
test <- spam[-index,]

#線形SVM
l.svm <- ksvm(type ~ ., data=train, kernel="vanilladot")
l.pre <- predict(l.svm, test)
(l.t <- table(l.pre, test$type))
(l.cr <- sum(diag(l.t))/sum(l.t))

##irisデータで可視化

#データ作成
#可視化のため変数をpetal.lengthとpetal.widthだけに落としている
#2値分類にするため、virginicaを削る
iris2 <- iris[1:100,]
index2 <- sample(1:100,80)
(train2 <- iris2[index2,c(-1,-2)])
(test2 <- iris2[-index2,c(-1,-2)])

#irisでカーネルSVM
iris.ksvm <- ksvm(Species ~ .,data=train2 )
iris.pre <- predict(iris.ksvm, test2)
(iris.t <- table(iris.pre, test2$Species))
(iris.cr <- sum(diag(iris.t))/sum(iris.t))
plot(iris.ksvm, data=iris2)

##spamデータをカーネル法で
#カーネルSVM(ガウシアンカーネル)
k.svm <- ksvm(type ~ ., data=train, kernel="rbfdot")
k.pre <- predict(k.svm, test)
(k.t <- table(k.pre, test$type))
(k.cr <- sum(diag(k.t))/sum(k.t))


#繰り返し処理用
l.cr <- NULL
k.cr <- NULL

for(i in 1:100){
  index <- sample(1:4601, 3000)
  train <- spam[index,]
  test <- spam[-index,]
  l.svm <- ksvm(type ~ ., data=train, kernel="vanilladot")
  l.pre <- predict(l.svm, test)
  l.t <- table(l.pre, test$type)
  l.cr[i] <- sum(diag(l.t))/sum(l.t)

  k.svm <- ksvm(type ~ ., data=train, kernel="rbfdot")
  k.pre <- predict(k.svm, test)
  k.t <- table(k.pre, test$type)
  k.cr[i] <- sum(diag(k.t))/sum(k.t)
}

mean(l.cr)
mean(k.cr)

#ランダムフォレストで重要度計算して
#重要そうな変数にあてをつける
library(randomForest)
rf <- randomForest(type~., data=train, importance=T)
rf
varImpPlot(rf)

pre.rf <- predict(rf, test)
pre.rf
t.rf <- table(pre.rf, test$type)
(cr.rf <- sum(diag(t.rf))/sum(t.rf))

#重要そうな変数だけでやってみる
l.cr <- NULL
k.cr <- NULL

for(i in 1:100){
  index <- sample(1:4601, 3000)
  train <- spam[index,]
  test <- spam[-index,]
  l.svm <- ksvm(type ~ charExclamation + capitalAve + hp + remove + charDollar + free + your, data=train, kernel="vanilladot")
  l.pre <- predict(l.svm, test)
  l.t <- table(l.pre, test$type)
  l.cr[i] <- sum(diag(l.t))/sum(l.t)
  
  k.svm <- ksvm(type ~ charExclamation + capitalAve + hp + remove + charDollar + free + your, data=train, kernel="rbfdot")
  k.pre <- predict(k.svm, test)
  k.t <- table(k.pre, test$type)
  k.cr[i] <- sum(diag(k.t))/sum(k.t)
}

#変数を絞ると違いが出てくる
mean(l.cr)
mean(k.cr)
