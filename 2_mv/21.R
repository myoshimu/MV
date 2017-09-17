#問1 給与データ読み込み、全変数の散布図作成
d<-read.csv("new.salary.csv")
head(d)
str(d)
#Xの削除
df<-d[,-1]
#散布図の作成
par(family="Osaka")
plot(df)

#問2 重回帰分析（全変数投入）
LM4 <- lm(給与~., data=df)
summary(LM4)

#問3 欠勤日だけP値が大きく検定を通過しない

#問4 欠勤日を抜いた式
LM.abs.out <- lm(給与~勤続年数+仕事量+特殊免許,data=df)
summary(LM.abs.out)
#ステップワイズ減少法による最適化
library(MASS)
LM4.aic <- stepAIC(LM4)
summary(LM4.aic)


#問7

