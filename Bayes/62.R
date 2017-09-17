library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options(max.print=10000)

#問62
#10回中8回勝った三浦選手が勝率が0.7以上になる確率
y <- 8 #勝利数
n <- 10 #試合数
data <- list(y,n)

MIURA <- '
data{
  int<lower=0> n; 
  int<lower=0> y; 
}
parameters {
  real<lower=0,upper=1> pi;
}
model {
  y ~ binomial(n, pi);
# 事前分布として一様分布＝Beta(1,1)を与える。
# 一度勝つとBeta(2,1)になる。8勝2敗するとBeta(9,3)になる。
#  pi~uniform(0,1);
# 去年三浦は12回投げて3回しか勝てなかったという情報を事前分布に与える場合
  pi~beta(4,10);
}
'

#MCMC
mc.chain <- stan(model_code=MIURA,data=data,iter=3000, warmup=1000)


#rstanのtraceplot関数でトレースプロット
traceplot(mc.chain)
mc.chain
pi.mcmc <- extract(mc.chain, pars=c("pi")) 
pi.mcmc
hist(pi.mcmc$pi)
sum(pi.mcmc$pi>0.7)/length(pi.mcmc$pi)
#pbetaでベータ分布(beta(9,3)の累積値を計算し1から引いて勝率7割以上の確率を調べてる
1-pbeta(0.7,9,3)