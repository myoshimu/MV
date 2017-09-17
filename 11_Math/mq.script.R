##Q1, カテゴリ回帰 ##
#データの読み込む
d <- read.csv("Mansion1.csv")
head(d)
str(d)

#カテゴリカル回帰分析
Q1 <- lm(家賃 ~ ., data=d)
summary(Q1)

#記述統計
summary(d)

##Q2, ダミー変数で線形判別 ##
#データの読み込み
d2 <- read.csv("Ryokan.csv", fileEncoding="cp932")
d2

#ダミー変数を使った、線形判別分析
library(MASS)
Q2 <- lda(再利用 ~ . , data=d2[-10,])
Q2

predict(Q2, d2[10,]) #10番目のデータでだけでホールドアウト


##Personality1
#データの読み込み
d3 <- read.csv("Personality1.csv", fileEncoding="cp932")
head(d3)
str(d3)

#ダミー変数を使った、線形判別分析
res <- lda(血液型 ~ . ,data=d3[-41,])
res

#自分の血液型は当たるか?
predict(res, d3[41,])


#例えばこんな感じで変数を絞ることも考えうる
library(randomForest)
rf <- randomForest(血液型~., data=d3, importance=T)
varImpPlot(rf)

#変数をAccuracy5位, Gini5位までとる。
res2 <- lda(血液型 ~ 現実的 + 堅実 + 気分屋 + 
               仲間意識 + 型にはまる +クール,
               data=d3[-41,])
res2

#今度は当たるか?
predict(res2, d3[41,])

##Q3 コレスポンデンス##
#データ作成
> Home <- matrix(c(41,23, 5, 5, 2,
                   +                 15, 4, 0, 2, 1,
                   +                  7,11, 2, 0, 0,
                   +                  3, 3, 0, 0, 0), 4, byrow=TRUE)
> rownames(Home) <- c("会社員","自営業","学生","その他")
> colnames(Home) <- c("マンション","ハイツ・コーポ","アパート","テラスハウス","一戸建て")

#コレスポンデンス分析
> library(MASS)
> COR1 <- corresp(Home, nf=2)
> COR1  # 結果の表示

##Q4 多次元尺度構成法 ##
#データの読み込み
d <- read.table("yamanote.txt",header = T)
rownames(d) <- colnames(d)
d

#計量MDS(非計量は距離sじゃないもの、類似度や相関係数でもできる)
MDS1 <- cmdscale(d)
MDS1 #主成分座標

#可視化
plot(MDS1, type="n")
par(family="HiraginoSans-W3")
text(MDS1, names(d), col="Orange")

#ヨーロッパで練習
data(eurodist)
labels(eurodist)

MDS2 <- cmdscale(eurodist, k=2)

#可視化
plot(MDS2, type="n")
text(MDS2, , labels(eurodist), col="blue")

#気持ち悪いので、主成分座標を反転、もちろん向きに意味はない
plot(MDS2[,1],-MDS2[,2], type="n")
text(MDS2[,1],-MDS2[,2], labels(eurodist), col="blue")





