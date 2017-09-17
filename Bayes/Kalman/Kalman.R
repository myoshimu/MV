d<-read.csv("rocket.csv")

#モデルの更新式（平均と分散）
mu <- NULL
sigma2 <- NULL

#パラメタの設定
a=1; b=1
#c=0.5
c=0
m=1; rho=1; tau=1

#初期状態の設定
mu[1] <- 0
#sigma2[1] <- 1
sigma2[1] <- 0.39

for(i in 2:21){
  #状態方程式で推移させる
  mu[i] = b*mu[i-1] + m
  sigma2[i] = b^2*sigma2[i-1] + c^2*rho^2
  #観測方程式で更新する
  mu[i] = (mu[i]*tau^2 + a*sigma2[i]*d$高度データ[i-1])/(tau^2+a*sigma2[i])
  sigma2[i] = (sigma2[i]*tau^2)/(tau^2+a*sigma2[i])
}

#更新の様子(分散が収束していく)
mu
sigma2

#高度データの先頭に0を追加
高度 <- append(d$高度データ,0,after=0)

#観測値と比べてみる
plot(高度, type="o",col=8, ylim=c(0,20))
par(new=T)
plot(mu,type="o", col=2, ylim=c(0,20), ylab="")
