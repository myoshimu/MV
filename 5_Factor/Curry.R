#データ読み込み
d2 <- read.csv("Curry.csv", fileEncoding="cp932")
head(d2)
str(d2)

#標準化
d2.s <- scale(d2[,-1])

#相関行列のチェック
cor(d2.s)

#Step1 factanal関数による因子分析2因子(rotation=noneで無回転)
fa2 <- factanal(d[,-1], factors=2, rotation="none")
print(fa2$loadings, sort=T) #因子負荷量が大きい順に並べ替え
#Loadings:の項目で因子パターン行列をみると、外観、雰囲気などの因子の係数（a1,a2..）がでてくる。
#f1は全て正の値なのでカレー屋の総合力みたいなものを示す
#***回転前の因子は主成分とほぼ同一になる***

#p.56 因子負荷量をplotしてみる(無回転)
par(mfrow=c(1,1))
plot(fa2$loadings[,1],fa2$loadings[,2],
     xlim=c(0,1), ylim=c(-1,1))
abline(a=0, b=0, lty=2, col=2) #この2行はx軸, y軸を書いているだけ
abline(v=0, lty=2, col=2)

#このままでは解釈しにくいので座標軸を回転したほうがわかりやすそう
#90度をたもったまま回転するvarimax法、斜交回転のpromax法がある
#今回はなんとなく90度でいけそうなのでvarimaxでやる

#p.57 factanal関数による因子分析(varimax回転)
fa2.vari <- factanal(d[,-1], factors=2, rotation="varimax")
#Loadingsの絶対値が0.1以下のところは値が消える
print(fa2.vari$loadings, sort=T)
#SS loadings(固有値)＝分散
#変数のすべての分散の合計（1*6=6）のうちf1,f2が占める割合

#因子負荷量をplotしてみる(回転後)
plot(fa2.vari$loadings[,1],fa2.vari$loadings[,2],
     xlim=c(0,1), ylim=c(-1,1))
abline(a=0, b=0, lty=2, col=2)
abline(v=0, lty=2, col=2)

#詳細
print(fa2, sort=T)
