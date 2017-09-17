#実際の例（レミゼラブル valuesは同じ章の同じ場面に現れた回数）
g<-read.graph("lesmis.gml",format="gml")
g
plot(g,weights=E(g)$values)

#（媒介）中心性　中心性には色々ある　固有ベクトル中心性とか
#あるノードに対して、その他の2点を結ぶ最短経路がどれくらいそのノードを通過しているか
g_bw <- betweenness(g,directed = F, weights=E(g)$value)
g_bw

#スピングラス法によるコミュニティ分析（サブグラフ検出）
#クラスター化といってもいい。クラスター度合いはQ値で測る
#Q値クラスタ内の辺密度が高く、クラスタ間の辺密度が低いと、高いQ値
#スピングラス法（別名焼きなまし法）
#Q_0 < Q_1なら採択
#Q_0>Q_1なら確率exp(\beta/Q_1-Q_0)で採択（かつbetaを徐々に大きくする＝温度を低下させる）

g_com<-spinglass.community(g,weights=E(g)$value,spins=50)
g_com

plot(g,vertex.size=log(50*g_bw+100),
     vertex.color=g_com$membership,
     layout=layout.fruchterman.reingold,margin=-0.2)
