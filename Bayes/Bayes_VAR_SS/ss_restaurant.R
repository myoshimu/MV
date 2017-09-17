#データの確認
d <- read.table("restaurant.txt", header=T)
head(d)
str(d)
summary(d)



install.packages("rstan")
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#わざと欠損期間を作ってみる
d <- read.table("restaurant.txt", header=T)
d[d$Month == 8, ]$Sale <- NA
Idx.na <- which(is.na(d$Sale))
Idx.obs <- which(!is.na(d$Sale))
Sale.obs <- d$Sale[Idx.obs]

#データの用意(リスト型)
data <- list(
  N = nrow(d),
  N_obs = length(Idx.obs),
  N_na = length(Idx.na),
  Idx_obs = Idx.obs,
  Idx_na = Idx.na,
  Wday = d$Wday,
  D1 = d$D1,
  D2 = d$D2,
  Rain_val = d$Rain_val,
  Event_val = d$Event_val,
  Sale_obs = Sale.obs
)

#kentaro matsuura(2013) BUGS/Stan勉強会を参考に少し修正
#https://www.slideshare.net/berobero11/ss-29409382
model1<- '
data {
  int<lower=1> N;
  int<lower=1> N_obs;
  int<lower=0> N_na;
  int<lower=1> Idx_obs[N_obs];
  int<lower=1> Idx_na[N_na];
  int<lower=0, upper=1> D1[N];
  int<lower=0, upper=1> D2[N];
  int<lower=0, upper=6> Wday[N];
  vector[N] Rain_val;
  vector[N] Event_val;
  vector<lower=0>[N_obs] Sale_obs;
}

parameters {
  vector[N] trend;
  vector[N] s;
  real<lower=0, upper=1> b1;
  real<lower=0, upper=1> b2;
  real<lower=0, upper=1> b3;
  real c_rain;
  vector[N] c_event;
  vector[N] ar;
  real c_ar[2];
  real<lower=0, upper=100> s_trend;
  real<lower=0, upper=100> s_s;
  real<lower=0, upper=100> s_event;
  real<lower=0, upper=100> s_ar;
  real<lower=0, upper=100> s_r;
}

transformed parameters {
  vector[N-6] sum_seg_s;
  vector[N] week;
  vector[N] rain;
  vector[N] event;
  vector[N] sale_mu;
  for (i in 7:N)
    sum_seg_s[i-6] = sum(s[i-6:i]);
  for(i in 1:7)
    week[i] = s[i] + D1[i]*b1*(s[i-Wday[i]]-s[i]);
  for(i in 8:N)
    week[i] = s[i] + D1[i]*b1*(s[i-Wday[i]]-s[i])
  + D2[i]*(b2*(s[i-Wday[i]-2]-s[i]) + b3*(s[i-Wday[i]-1]-s[i]));
  rain = c_rain * Rain_val;
  event = c_event .* Event_val;
  sale_mu = trend + week + rain + event + ar;
}

model {
  trend[3:N] ~ normal(2*trend[2:N-1] - trend[1:N-2], s_trend);
  sum_seg_s ~ normal(0, s_s);
  c_event[2:N] ~ normal(c_event[1:N-1], s_event);
  ar[3:N] ~ normal(c_ar[1]*ar[2:N-1] + c_ar[2]*ar[1:N-2], s_ar);
  Sale_obs ~ normal(sale_mu[Idx_obs], s_r);
}

generated quantities {
  vector[N_na] sale_na;
  for (i in 1:N_na)
    sale_na[i] = normal_rng(sale_mu[Idx_na[i]], s_r);
}
'

#MCMC 時間2:00-3:58
#使用したPCのスペック
#CPU:3.1GHz intel-Corei7, RAM:12GB, GPU:AMD RadeonHD 6970M 1024MB
#MCMベイズによるベイズ推定
fit <- stan(model_code=model1, data=data,
            pars=c('trend', 's', 'b1', 'b2', 'b3', 'c_rain', 'c_event', 'ar', 'c_ar', 's_trend', 's_s', 's_event', 's_ar', 's_r'),
            iter=6000, warmup=1000, thin=5)
           
#事後期待値
trend <- get_posterior_mean(fit, pars="trend")[, 'mean-all chains']
get_posterior_mean(fit, pars="s")[, 'mean-all chains']
get_posterior_mean(fit, pars="b1")[, 'mean-all chains']
get_posterior_mean(fit, pars="b2")[, 'mean-all chains']
get_posterior_mean(fit, pars="b3")[, 'mean-all chains']
get_posterior_mean(fit, pars="c_rain")[, 'mean-all chains']
c_event <- get_posterior_mean(fit, pars="c_event")[, 'mean-all chains']
get_posterior_mean(fit, pars="ar")[, 'mean-all chains']
get_posterior_mean(fit, pars="c_ar")[, 'mean-all chains']
get_posterior_mean(fit, pars="s_trend")[, 'mean-all chains']
get_posterior_mean(fit, pars="s_s")[, 'mean-all chains']
get_posterior_mean(fit, pars="s_event")[, 'mean-all chains']
get_posterior_mean(fit, pars="s_ar")[, 'mean-all chains']
get_posterior_mean(fit, pars="s_r")[, 'mean-all chains']

#こんな感じで分解したものの和が取れる
plot(Sale.obs, type="l", ylim=c(0,250))
par(new=T)
plot(trend, type="l",col="red", ylim=c(0,250))
par(new=T)
plot(c_event, type="l",col="green", ylim=c(0,250))
