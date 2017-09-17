iris
iris[c(1,51,101),]

#setosaとvirginicaを取り出す, select=-SpeciesでSpecies列を削除
seto<-subset(iris[1:50,],select=-Species)
virgi<-subset(iris[101:150,],select=-Species)

#2種ごとの各列の平均を求める（45行目までが教師データ）
seto.m<-apply(seto[1:45,],2,mean)
virgi.m<-apply(virgi[1:45,],2,mean)
seto.m
virgi.m

#2種の分散共分散行列を求める
seto.v<-var(seto[1:45,])
virgi.v<-var(virgi[1:45,],)
seto.v
virgi.v

#マハラノビス汎距離を計算
D1 <- mahalanobis(seto[46:50,],seto.m,seto.v)

