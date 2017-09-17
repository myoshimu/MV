#アソシエーション分析
library(arules)

#練習
test <- list(c("bread","milk","ham","fruit"),
             c("bread","diapers","beer","ham"),
             c("sausage","beer","diapers"),
             c("bento","beer","diapers","tobacco"),
             c("bread","beer","juice","fruit"))

test
test[1]
test[[1]]

#トランザクションデータ
test.tran <- as(test,"transactions")
test.tran@itemInfo
test.tran@data

#itemMatrix形式
as(test.tran, "matrix")

#データフレーム
as(test.tran,"data.frame")

#相関ルールの抽出（ルール70個）
#たった5個で70ルールなので全部やるのは現実的ではない
test.ap <- apriori(test.tran)
inspect(test.ap)

#supportは支持度、confidenceは確信度
#sigma(X)を集合Xを含むバスケット数と定義
inspect(head(sort(test.ap, by="support"),n=10))

#ルールの抽出規則変更
test.ap2 <- apriori(test.tran, parameter = list(supp=0.4,maxlen=3))
inspect(test.ap2)

#パッケージのデータ使用
data(Groceries)
Groceries
as(Groceries,"matrix")

#barplot相対度数
par(mar=c(4.5, 2,1,2), cex=0.65, cex.axis=0.7)
itemFrequencyPlot(Groceries[,1:55], cex=0.65, col="lightblue", horiz=T)
itemFrequencyPlot(Groceries[,56:110], cex=0.65, col="lightblue", horiz=T)
itemFrequencyPlot(Groceries[,111:169], cex=0.65, col="lightblue", horiz=T)

#ルールの抽出
Gr.ap <- apriori(Groceries) #アイテムたくさんすぎてルールが抽出できない

#ルール
Gr.ap2 <-apriori(Groceries,parameter=list(supp=0.05, confidence=0.05))
inspect(Gr.ap2)

#肉に関するルールだけ取ってくる
beef <- subset(Gr.ap2, subset=rhs %in% "beef")
inspect(beef)

