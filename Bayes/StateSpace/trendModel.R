library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())


trendModel <- "
data{
  int<lower=0> N;
  real<lower=0> Y[N]; //その時の年輪の幅
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

#data
X <- 1961:1990
Y <- c(4.71, 7.70, 7.97, 8.35, 5.70,
       7.33, 3.10, 4.98, 3.75, 3.35,
       1.84, 3.28, 2.77, 2.72, 2.54,
       3.23, 2.45, 1.90, 2.56, 2.12,
       1.78, 3.18, 2.64, 1.86, 1.69,
       0.81, 1.02, 1.40, 1.31, 1.57)
data <- list(N=length(X), Y=Y)

#MCMC
stantrend <- stan_model(model_code=trendModel)
fit_trend <- sampling(stantrend, data=data, iter=5000, warmup=500, thin=3, seed=1)
fit_trend
traceplot(fit_trend, pars="alpha")
traceplot(fit_trend, pars=c("s_Y", "s_a"))

#パラメータの取り出し
alpha <- get_posterior_mean(fit_trend, par='alpha')[, 'mean-all chains']
s_Y <- get_posterior_mean(fit_trend, par='s_Y')[, 'mean-all chains']
s_a <- get_posterior_mean(fit_trend, par='s_a')[, 'mean-all chains']

#信頼上限加限
upr <- alpha + 1.96*summary(fit_trend)$summary[1:30,3]
lwr <- alpha - 1.96*summary(fit_trend)$summary[1:30,3]

#描画
res <- data.frame(Y, alpha, upr, lwr)
matplot(res,type="l", lty=1:4, col=1:4)
legend("topright",c("observe","state","95%upr","95%lwr"),lty=1:4,col=1:4)
