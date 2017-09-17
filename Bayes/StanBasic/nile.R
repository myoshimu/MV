localLevelModel_1 <- "
  data{
    int n;
    vector[n] Nile;
  }
  parameters{
    real mu;
    real<lower=0> sigmaV;
  }
  model{
    for(i in 1:n){
      Nile[i]~normal(mu, sqrt(sigmaV));
    }
  }
"

Nile
NileData <- list(Nile = as.numeric(Nile), n=length(Nile))
NileData
#ここからMCMC
set.seed(1)
NileModel1 <- stan(
  model_code = localLevelModel_1,
  data = NileData,
  iter = 1100,
  warmup = 100,
  thin = 1, #間引き（1は間引きしないということ）
  chains = 3 #初期値に依存しないようバリエーションを設定、3-4が一般的でデフォルト4
)
NileModel1
traceplot(NileModel1)
