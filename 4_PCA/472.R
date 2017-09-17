mtcars #R内のデータ(1973-74車の分析)
?mtcars

#prcomp利用
p.mt <-prcomp(mtcars, scale = TRUE)
p.mt

#累積寄与率(2因子で84%いっている)
cumsum(p.mt$sdev^2)/11

#バイプロット(cexは文字サイズ指定)
biplot(p.mt, cex=0.7) 

#第一主成分はcyl,disp,hp,wt,carbの影響が大きく
#燃費はマイナスなので馬力のような要素になると考えられる。
#第二主成分はam,gear,carbの影響が強く
#技巧的な扱いやすさを示す？