library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options(max.print=10000)

#問65
#20人の学生について10点満点のテストを行った結果から適切なモデルを考える。
#学生番号、点数

score<-c(1,0,10,4,10,10,10,6,4,10,1,9,0,5,10,7,1,9,2,8)
score.list
hist(score)

h.Model1 <- '
data{
  int<lower=0> N; //サンプルサイズ
  int<lower=0> X[N]; //スコア
}
parameters {
  real beta; //全個体共通のロジスティック変回帰係数（共通部分）
  real r[N];
  real<lower=0> s;
}
transformed parameters {
  real q[N];
  for (i in 1:N)
  q[i] = inv_logit(beta+r[i]);
}
model {
  for (i in 1:N)
    X[i] ~ binomial(10,q[i]);
    beta~normal(0,100);
  for (i in 1:N)
    r[i] ~ normal(0,s);
  s~uniform(0,10000);
}
'

#MCMC
stan.hieral <- stan_model(model_code=h.Model1)
fit.hieral <- sampling(stan.hieral, data=score.list, iter=3000, warmup=1000)
fit.hieral

traceplot(fit.hieral, pars="beta")
traceplot(fit.hieral, pars="s")
beta <- get_posterior_mean(fit.hieral, par='beta')[,'mean-all chains']
r <- get_posterior_mean(fit.hieral, par='r')[,'mean-all chains']
q <- get_posterior_mean(fit.hieral, par='q')[,'mean-all chains']

