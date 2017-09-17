#就職アンケートの相関行列をみてみる
d<-read.csv("Shinsotsu.csv",fileEncoding="cp932")
head(d)
str(d)
#伝統と有名などいくつかの要素が似ている
cor(d[,-1])

#カレー屋アンケートで因子分析
d2<-read.csv("Curry.csv", fileEncoding = "cp932")
head(d2)
str(d2)

#標準化
d2.s <- scale(d2[,-1])
