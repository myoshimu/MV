#データの読み込みと確認
d <- read.csv("new.salary.csv")
head(d)
str(d)

#Xの削除
df <- d[,-1]
head(df)

#散布図作成
par(family="Osaka")　#macの人は日本語フォント指定
plot(df)

#重回帰分析(全変数投入)
LM4 <- lm(給与 ~ ., data=df)
summary(LM4)

#欠勤日を抜いた式
LM.abs.out <- lm(給与 ~ 勤続年数 + 仕事量 + 特殊免許, data=df)
summary(LM.abs.out)

#ステップワイズ減少法による最適化(AIC)
library(MASS)
LM4.aic <- stepAIC(LM4)
summary(LM4.aic)

#多重共線性の検討
cor(df)

#残差の検討・正規性の検討・外れ値の検討
plot(LM4.aic)