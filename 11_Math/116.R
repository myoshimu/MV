#多次元尺度構成法（数量4類）
#二ー三次元の距離を測る方法（クラスタ分析同様）

d<- read.table("yamanote.txt", header=T)
#距離行列（対称）
rownames(d) <- colnames(d)
d

#軽量MDS
MDS1 <- cmdscale(d)
MDS1 #主成分座標

plot(MDS1, type="n")
par(family="HiraginoSans-W3")
text(MDS1, names(d), col="Orange")
