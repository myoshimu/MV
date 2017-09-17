##焼酎の例題
#データの確認
d <- read.csv("syoucyu.csv", fileEncoding="cp932")
head(d)
str(d)
summary(d)

#散布図
par(family="HiraginoSans-W3") #mac
plot(d) #わからないので一つずつ

par(mfrow = c(1,2))
plot(d$林伊蔵 ~ d$最高気温)
plot(d$林伊蔵 ~ jitter(d$最高気温)) #誤差加えて見やすく

plot(d$林伊蔵 ~ d$火金土)
plot(d$林伊蔵 ~ jitter(d$火金土)) #誤差加えて見やすく

#一般化線形回帰(GLM)で二項ロジスティック回帰
logi.rg <- glm(林伊蔵 ~ 火金土 + 最高気温, data=d,
                  family=binomial(link="logit"))
logi.rg

#判別正答率を出す
p <- predict(logi.rg, type="response")
#注意　type="response"が無いとpredictの返す値は線形予測子の値になる
yp <- ifelse(p < 0.5, "0", "1") #確率0.5未満を0,それ以外1
yp
t <- table(d$林伊蔵, yp) #テーブル
t
cr <- sum(diag(t))/sum(t)　#判別正答率(予測正答率でない)
cr

#回帰係数の検定など
summary(logi.rg)

#最高気温23度土曜日の予測
new.d <- data.frame(最高気温 = 23, 火金土 = 1)
new.d
predict(logi.rg, new.d, type="response")

#オッズ比の計算
exp(logi.rg$coefficients)[-1]
