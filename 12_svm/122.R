#irisでカーネルSVMを可視化
#2値分類にして等高線プロットを描くためvirginicaは削る
iris2 <- iris[1:100,]
index2 <- sample(1:100,80)
#irisデータを2次元に落とし込む
(train2<-iris2[index2,c(-1,-2)])
(test2<-iris2[-index2,c(-1,-2)])

#irisでカーネルSVM
iris.ksvm<-ksvm(Species~.,data=train2)
iris.pre<-predict(iris.ksvm, test2)
(iris.t<-table(iris.pre,test2$Species))
(iris.cr <- sum(diag(iris.t)/sum(iris.t)))
plot(iris.ksvm,data=iris2)

