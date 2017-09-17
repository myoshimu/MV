library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())

#ローカルレベルモデル
localLevelModel <- '
  data{
    int<lower=0> N;
    real<lower=0> Y[N];
  }

  parameters {
    real alpha[N];
    real<lower=0> s_Y;
    real<lower=0> s_a;
  }

  model {
    for(i in 1:N)
      Y[i] ~ normal(alpha[i], s_Y);
    for(i in 2:N)
      alpha[i] ~ normal(alpha[i-1], s_a);
  }
'

#MCMC
stanlocal <- stan_model(model_code=localLevelModel)
fit_local <- sampling(stanlocal, data=data, iter=1700, warmup=200, seed=1)

fit_local
traceplot(fit_local)

#パラメータの取り出し
alpha <- get_posterior_mean(fit_local, par='alpha')[,'mean-all chains']
s_Y <- get_posterior_mean(fit_local, par='s_Y')[,'mean-all chains']
s_a <- get_posterior_mean(fit_local, par='s_a')[,'mean-all chains']

upr <- alpha + 1.96*summary(fit_local)$summary[1:30,3]
lwr <- alpha - 1.96*summary(fit_local)$summary[1:30,3]

#描画
res <- data.frame(Y, alpha, upr, lwr)
matplot(res, type="l", lty=1:4, col=1:4)
legend("topright", c("observe", "state", "95%upr", "95%lwr"), lty=1:4, col=1:4)