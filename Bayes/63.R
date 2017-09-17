library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options(max.print=10000)

#問63
#以下のコードで生成された変数X,Yについて、t検定で平均の差について考察。
#平均7,標準偏差6の乱数を100生成
set.seed(5)
 x<-rnorm(100,7,6)
 y<-rnorm(100,6,2)
 
mean(x)
sd(x)
mean(y)
sd(y)

#t検定
t.test(x,y,var.equal=T)

t.model <- '
data{
  real x[100];
  real y[100];
}
parameters {
  real mu_x;
  #real mu_y;
  real diff;
  real<lower=0> sigma_x;
  real<lower=0> sigma_y;
}
model {
  x~normal(mu_x, sigma_x);
  y~normal(mu_x+diff, sigma_y);
}
'

#MCMC
data.t<-list(x,y)
t.mcmc <- stan(model_code = t.model, data=data,iter=30000, warmup=10000)
traceplot(mc.chain)
t.mcmc

diff.mcmc <- extract(t.mcmc, pars=c("diff"))
hist(diff.mcmc$diff)