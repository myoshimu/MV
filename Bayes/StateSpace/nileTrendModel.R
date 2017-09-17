library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())


nileTrendModel <- "
data{
  //vector[n] Nile;
  int<lower=0> N;
  int<lower=0> Y[N]; //その時のナイル川の量
}
parameters{
  real alpha[N];
  real<lower=0> s_Y;
  real<lower=0> s_a;
}
model{
for(i in 1:N){
  Y[i]~normal(alpha[i], s_Y);
}
for(i in 3:N) //t-2期からはじまるため3:N
    alpha[i] ~ normal(2*alpha[i-1] - alpha[i-2], s_a);
}
"

data <- list(Y = as.numeric(Nile), N=length(Nile))
data
#ここからMCMC
stantrend <- stan_model(model_code=nileTrendModel)
fit_trend <- sampling(stantrend, data=data, iter=3000, warmup=500, thin=3, seed=1)
fit_trend

traceplot(fit_trend, pars="alpha")
traceplot(fit_trend, pars=c("s_Y", "s_a"))

#パラメータ事後分布の取り出し
alpha <- get_posterior_mean(fit_trend, par='alpha')[, 'mean-all chains']
s_Y <- get_posterior_mean(fit_trend, par='s_Y')[, 'mean-all chains']
s_a <- get_posterior_mean(fit_trend, par='s_a')[, 'mean-all chains']

#信頼上限加限
upr <- alpha + 1.96*summary(fit_trend)$summary[1:100,3]
lwr <- alpha - 1.96*summary(fit_trend)$summary[1:100,3]

#描画
res <- data.frame(Y, alpha, upr, lwr)
matplot(res,type="l", lty=1:4, col=1:4)
legend("topright",c("observe","state","95%upr","95%lwr"),lty=1:4,col=1:4)
