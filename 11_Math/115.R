#コレスポンデンス分析（数量三類、クロス表を使った分析でよく使う）

#データ作成
Home <- matrix(c(41,23, 5, 5, 2,
                 15, 4, 0, 2, 1,
                 7, 11, 2, 0,0,
                 3, 3, 0, 0, 0), 4, byrow=TRUE)
rownames(Home) <- c("会社員","自営業","学生","その他")
colnames(Home) <- c("マンション","ハイツ・コーポ","アパート","テラスハウス","一戸建て")

library(MASS)
COR1 <- corresp(Home, nf=2)
COR1
par(family="Osaka")
biplot(COR1)


