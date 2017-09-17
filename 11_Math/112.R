d <- read.csv("Mansion1.csv")
head(d)
str(d)

#カテゴリカル回帰分析（数量１類、Rにお任せ＝正しい方法）
Q1 <- lm(家賃~., data=d)
summary(Q1)

#上の式では間取り2LDKがもっとも大きい負の係数になってしまう
plot(Q1)
library(MASS)
Q1.aic<-stepAIC(Q1)

#記述統計
summary(d)