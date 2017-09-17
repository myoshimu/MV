# 並列化演算を行う
# install.packages("doParallel") コア数は自分で調べて
library(doParallel)
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

install.packages("adabag")
install.packages("randomForest")
install.packages("caret")

library(kernlab)
data(spam)
index <- sample(1:4601, 2500)
train <- spam[index,]
test <- spam[-index,]

#bagging(bootstrap aggregating)
#各学習器は独立
#並行化が容易
library(adabag)
spam.ba <- bagging(type ~ ., data = train)
spam.ba
spam.ba.pre <- predict(spam.ba, test)
spam.ba.pre$confusion
1-spam.ba.pre$error
spam.ba$trees

#boosting
#間違えたものをより敏感に感知するように重み付け
#並行化は難しい
spam.bst <- boosting(type~., data=train)
spam.bst.pre <- predict(spam.bst, test)
spam.bst.pre$confusion
1-spam.bst.pre$error

#randomForest(mtryはsqrt(変数))で決めてるっぽい
library(randomForest)
spam.rf <- randomForest(type~., data=train, importance=T)
spam.rf.pre <- predict(spam.rf,test)
t.rf <- table(test$type, spam.rf.pre)
cr.rf <- sum(diag(t.rf))/sum(t.rf)
cr.rf

plot(spam.rf)
importance(spam.rf)
varImpPlot(spam.rf)

#もう少しrandomForest
tuneRF(spam[,-58],spam[,58], doBest=T)
spam.rf.tune <- randomForest(type~., data=train, mtry=7) #mtryが同じでも結果は違う
spam.rf.tune
importance(spam.rf.tune)
t.rf.tune <- table(test$type, predict(spam.rf.tune, test))
t.rf.tune
cr.rf.tune <- sum(diag(t.rf.tune))/sum(t.rf.tune)
cr.rf.tune




#ワインデータセット
library("httr")
geturl <- GET("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")
dat <- read.csv(textConnection(content(geturl)), header=F)
dat

head(dat)
names(dat) <- c("class", paste0("V", 1:13))
dat <- transform(dat, class = as.factor(class))
str(dat)

#V1) Alcohol	アルコール
#V2) Malic acid	リンゴ酸
#V3) Ash	灰
#V4) Alcalinity of ash	灰のアルカリ性
#V5) Magnesium	マグネシウム
#V6) Total phenols	フェノール類全量
#V7) Flavanoids	フラバノイド
#V8) Nonflavanoid phenols	非フラバノイドフェノール類
#V9) Proanthocyanins	プロアントシアニン
#V10)Color intensity	色彩強度
#V11)Hue	色調
#V12)OD280/OD315 of diluted wines	蒸留ワインのOD280/OD315
#V13)Proline	プロリン

index <- sample(1:178, 118)
train <- dat[index,]
test <- dat[-index,]

wine.rf <- randomForest(class ~ ., data=train, importance=T)
wine.rf
wine.rf.pre <- predict(wine.rf, test)
wine.rf.t <- table(wine.rf.pre, test$class)
wine.rf.t
wine.rf.cr <- sum(diag(wine.rf.t))/sum(wine.rf.t)
wine.rf.cr
importance(wine.rf)


#最近はranger/Rboristっていうのが早いらしい
install.packages(c("randomForest", "ranger", "Rborist", "pipeR"))
library(randomForest)
library(ranger)
library(Rborist)
library(kernlab)
library(pipeR)

data(spam)
index <- sample(1:4601, 2500)
train <- spam[index,]
test <- spam[-index,]

system.time(randomForest(type~., train, ntree=1000))
system.time(ranger(type~., train, num.trees = 1000, seed=71))
system.time(ranger(type~., train, seed=71, save.memory=TRUE))
system.time(Rborist(train[,-58], train[,58], nTree=1000))
