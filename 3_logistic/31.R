#ロジスティック回帰（焼酎の売上が曜日、気温で変わるか）
#データの確認
d<-read.csv("syoucyu.csv",fileEncoding="cp932")
head(d)
str(d)
summary(d)

#散布図
par(family="HiraginoSans-W3")
plot(d)

par(mfrow=c(2,2))
plot(d$林伊蔵~d$最高気温)
plot(d$林伊蔵~jitter(d$最高気温)) #誤差を加えて見やすく

plot(d$林伊蔵~d$火金土)
plot(d$林伊蔵~jitter(d$火金土)) #誤差を加えて見やすく

#一般化線形回帰で二項ロジスティク回帰
logi.rg <- glm(林伊蔵~火金土+最高気温, data = d, family=binomial(link="logit"))
logi.rg

#計算してみる（土曜、23度）と0.44程度ー＞売れるか微妙？
1/(1+exp(15.2035-2.4426*1-0.5445*23))

#判別正答率(21日文の予測値)
p <- predict(logi.rg, type="response")
#注意 type="response"がないとpが線形予測子になる
yp <- ifelse(p<0.5, "0", "1") #確率0.5未満は0, それ以外1
yp
t <- table(d$林伊蔵, yp) #テーブル
t
cr <- sum(diag(t)/sum(t)) #判別正答率（予測正答率ではない）
cr

summary(logi.rg)

#最高気温23度、土曜日の予測
new.d <- data.frame(最高気温 = 23, 火金土= 1)
new.d
predict(logi.rg, new.d, type="response")

#オッズ比
exp(logi.rg$coefficients)[-1]
