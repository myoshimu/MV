library(h2o)
library(kernlab)
data(spam)

#トレーニングとテストに分割
train_index <- sample(1:4601, 3000)
train_data <- spam[train_index,]
test_data <- spam[-train_index,]

h2o.init()

#h2o形式に変換
train.h2o <- as.h2o(train_data)
test.h2o <- as.h2o(test_data)

#deep learning(100x3層の隠れ層を持つニューラルネット)
res.d1 <- h2o.deeplearning(x=1:57, y=58,
                           activation="RectifierWithDropout",
                           train.h2o,hidden=c(100,100,100),epochs=100)

#予測
(pred.d1 <- h2o.predict(object=res.d1, newdata=test.h2o[,-58]))
(pred.d1.df <- as.data.frame(pred.d1))

#正答率計算
(t <- table(test_data$type, pred.d1.df[,1]))
(cr <- sum(diag(t))/sum(t))

#h2o終了
h2o.shutdown()prompt = Y

#SVMと比較
res <- ksvm(type~., data=train_data)
(pred2 <- predict(res, test_data[,-58]))
(t2 <- table(test_data$type, pred2))
(cr2 <- sum(diag(t2))/sum(t2))
