#ニューラルネットワーク
library(nnet)

#データの読み込みと確認
#CV:購入, a1~a7:ウェブ販促ページを見たか
d.cv<-read.table("nndata.txt", header=T)
head(d.cv)
str(d.cv)

#トレーニングとテストを乱数で振り分け
index <- sample(1:3000,2000)
train <- d.cv[index,]
test <- d.cv[-index,]

#ニューラルネット（隠れ層はsizeで指定）
d.nnet <- nnet(cv~.,data=train,size=5)

#判別結果　50番目まで例示
predict(d.nnet, test[,-8])[1:50,]
round(predict(d.nnet, test[,-8]),0)[1:50,]

#判別正答率を計算するテーブル作成
t <- table(test$cv, round(predict(d.nnet, test[,-8]),0))
t

#判別正答率計算
cr <-sum(diag(t))/sum(t)
cr

#重みの詳細
summary(d.nnet)

#図示
source("http://hosho.ees.hokudai.ac.jp/~kubo/log/2007/img07/plot.nn.txt")
plot.nn(d.nnet)

#上の処理の繰り返し(損失関数でだんだん数値が減る)
cr <- NULL
for(i in 1:100){
  index <- sample(1:3000,2000)
  train <- d.cv[index,]
  test <- d.cv[-index,]
  
  d.nnet <- nnet(cv~.,data=train,size=5)
  t <- table(test$cv, round(predict(d.nnet, test[,-8]),0))
  cr[i] <- sum(diag(t))/sum(t)
}

#平均判別正答率
mean(cr)

#ちなみに他の手法(SVM)では？
library(kernlab)

cr.svm <- NULL
for(i in 1:100){
  index <- sample(1:3000, 2000)
  train <- d.cv[index,]
  test <- d.cv[-index,]
  
  d.svm <-ksvm(cv~.,data=train)
  t.svm <- table(test$cv, predict(d.svm, test[,-8]))
  cr.svm[i] <- sum(diag(t.svm))/sum(t.svm)
}

#平均判別正答率
mean(cr)
mean(cr.svm)
