library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options(max.print=10000)

#データ読み込み
#8個中の生存種子数
d <- read.csv(url("http://hosho.ees.hokudai.ac.jp/~kubo/stat/iwanamibook/fig/hbm/data7a.csv"))
head(d)
str(d)



h.Model1 <- '
data{
  int<lower=0> N; //サンプルサイズ
  int<lower=0> y[N]; //種子8個あたりの生存数(目的変数)
}
parameters {
  real beta; //全個体共通のロジスティック変回帰係数（共通部分）
  real r[N]; //個体差
  real<lower=0> s;
}
transformed parameters {
  real q[N];
  for (i in 1:N)
    q[i] = inv_logit(beta+r[i]); //生存確率を個体差でロジット変換
}
model {
  for (i in 1:N)
    y[i] ~ binomial(8,q[i]); //二項分布で生存確率をモデリング
  beta~normal(0,10000); //ロジスティック回帰係数の無情報事前分布
  for (i in 1:N)
    r[i] ~ normal(0,s); //個体差の階層事前分布
  s~uniform(0,10000); //r[i]を表現するための無情報事前分布
}
'

dat<-list(N=nrow(d),y=d$y)

#MCMC
stan.hieral <- stan_model(model_code=h.Model1)
fit.hieral <- sampling(stan.hieral, data=dat, iter=3000, warmup=1000)
fit.hieral

#rstanのtraceplot関数でトレースプロット
rstan::traceplot(fit.hieral,pars="beta")
rstan::traceplot(fit.hieral,pars="s")

#パラメータの平均取得
beta <- get_posterior_mean(fit.hieral, par = 'beta')[,'mean-all chains']
s <- get_posterior_mean(fit.hieral, par = 's')[,'mean-all chains']
r <- get_posterior_mean(fit.hieral, par = 'r')[,'mean-all chains']
q <- get_posterior_mean(fit.hieral, par = 'q')[,'mean-all chains']
beta
s
r
q
hist(q*8)

#rstan::extract関数で中身を取り出せる
#事前分布が一様分布だったsの事後分布をみてみるとベータ分布になってる
hist(rstan::extract(fit.hieral)$s)