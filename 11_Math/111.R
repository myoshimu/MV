#数量一類（ダミー変数使った分析、ダメな例）
x <- gl(5,4, labels=c("EAST","WEST","NORTH","SOUTH","UNKNOWN"))
x

lvs <- levels(x)
lvs

nlv <- nlevels(x)
nlv

x.dmy <- data.frame(matrix(0, nrow=20, ncol=5))
x.dmy

for (i in 1:nlv) {
  x.dmy[,1] <- as.integer(x==lvs[i])
}
data.frame(x.dmy,x)