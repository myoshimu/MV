##Titanicの例題
#データをみる
Titanic
d.t <- data.frame(Titanic)
d.t

#rawデータへの変換
d.raw <- data.frame(
  Class = rep(d.t[,1], d.t[,5]),
  Sex = rep(d.t[,2], d.t[,5]),
  Age = rep(d.t[,3], d.t[,5]),
  Survived = rep(d.t[,4], d.t[,5])
)
head(d.raw, 50)
str(d.raw) #2201レコード

#トレーニングデータをテストデータを分ける
index <- sample(1:2201, 1500) #非復元抽出で1:2201から一様乱数
train <- d.raw[index,]
test <- d.raw[-index,]
head(index)
head(train)

#ロジスティック回帰
lrg.tit <- glm(Survived ~ Class, data=train, family=binomial)
lrg.tit
summary(lrg.tit)

#予測正答率
p <- predict(lrg.tit, test, type="response")
yp <- ifelse(p < 0.5, "No", "Yes")
t <- table(test$Survived, yp)
t
cr <- sum(diag(t))/sum(t)
cr

#繰り返し処理(for文を用いた)
cr <- NULL
for(i in 1:100){
  index <- sample(1:2201, 1500)
  train <- d.raw[index,]
  test <- d.raw[-index,]
  
  lrg.tit <- glm(Survived ~ Class, data=train, family=binomial)
  
  p <- predict(lrg.tit, test, type="response")
  yp <- ifelse(p < 0.5, "No", "Yes")
  t <- table(test$Survived, yp)
  cr[i] <- sum(diag(t))/sum(t)
}

#予測正答率
cr
mean(cr) #平均予測正答率