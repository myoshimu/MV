bp<-read.csv("bp.csv")
colnames(bp) <- c("day","blooldPressure")
bp
medicine<-c(rep(0,10),rep(1,60),rep(2,30),rep(3,10))
medicine
plot(bp$bloodPressure,type="b")