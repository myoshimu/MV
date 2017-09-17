localLevelModel_2 <- "
  data{
    int n;
    vector[n] Nile;
    vector[n] t;
  }
  parameters{
    real a;
    real b;
    real<lower=0> sigma;
  }
  model{
    for(i in 1:n){
      Nile[i]~normal(a + b*t, sqrt(sigma));
  }
}
"

Nile
NileData <- list(Nile = as.numeric(Nile), n=length(Nile))
t <- 1:100
NileData
#ここからMCMC
set.seed(1)
NileModel2 <- stan(
  model_code = localLevelModel_2,
  data = NileData,
  iter = 2000,
  warmup = 800,
  thin = 1, #間引き（1は間引きしないということ）
  chains = 3 #初期値に依存しないようバリエーションを設定、3-4が一般的でデフォルト4
)
NileModel2
traceplot(NileModel2)

#lm(formula = as.numeric(Nile) ~ t)と結果が違う
