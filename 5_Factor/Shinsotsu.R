#p.62 就職アンケートのバートレット法分析
d<-read.csv("Shinsotsu.csv",fileEncoding="cp932")
head(d)
str(d)

#3因子モデルにプロマックス回転を使い、バートレット法で因子得点出す
fa3 <- factanal(d[,-1], factors=3,
                rotation="promax", scores="Bartlett")
print(fa3, sort=T) 　　#因子負荷量を並び替えて実行
#f1は会社が与えてくれるもの、f2は安定性、f3はベンチャー気質

#Step4精度
#寄与率と独自性の関係
mean(fa3$uniquenesses)+0.843 #0.843 は累積寄与率
round(mean(fa3$uniquenesses)+0.843)

#因子スコア
fa3$scores

#2 因子との比較
fa.2 <- factanal(d[,-1], factors=2,
                 rotation="promax", scores="Bartlett")
print(fa.2, sort=T)
#Uniquenessesが一部高いものがあると因子数を増やすといい
