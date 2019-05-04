par(family = "HiraginoSans-W4")
koeppen <- read.csv(file="world_temp_prec.csv", row.names=1, header=TRUE, fileEncoding="utf8") 
## 4.1クラスター分析
## 4.1.1 各月の気温と降水量のデータを使ったとき
mydata <- subset(koeppen, select=-c(class,height,t_year,p_year)) # 各月の気温と降水量のデータを使う。
## 階層的クラスタリングの実行:
dst <- dist(scale(mydata)) # ユークリッド距離
est <- hclust(dst, method="ward.D2") # Ward法 (代表的な方法の一つ)
plot(est)
## 結果の確認 (各クラスター内の都市名を表示)
clust <- cutree(est,5) # 気候区分を5つに分ける。
perf <- rownames(mydata) # 都市名の取得
perf[clust==1] # Af（熱帯雨林気候）
perf[clust==2] # A（Af以外）とC
perf[clust==3] # BとCとD
perf[clust==4] # BSとBW
perf[clust==5] # ET（ツンドラ気候）

## 4.1.2 高度、各月の気温と降水量のデータを使ったとき
mydata <- subset(koeppen, select=-c(class,t_year,p_year)) # 高度のデータを加えてみる。
## 階層的クラスタリングの実行:
dst <- dist(scale(mydata)) # ユークリッド距離
est <- hclust(dst, method="ward.D2") # Ward法 (代表的な方法の一つ)
plot(est)
## 結果の確認 (各クラスター内の都市名を表示)
clust <- cutree(est,6) # 気候区分を6つに分ける。
perf <- rownames(mydata) # 都市名の取得
perf[clust==1] # Af（熱帯雨林気候）
perf[clust==2] # A（Af以外）とC
perf[clust==3] # B（乾燥帯気候）
perf[clust==4] # CとD
perf[clust==5] # ET（ツンドラ気候）
perf[clust==6] # H（高山気候）

## 4.2 主成分分析
est <- prcomp(mydata, scale=TRUE) # 主成分分析の実行（4.1.2のmydataを使用）
biplot(est,cex=c(0.6, 0.7), scale=0) # バイプロット(第1 vs 第2主成分)
biplot(est,choices=c(2,3),cex=c(0.6, 0.7), scale=0) # バイプロット(第2 vs 第3主成分)

## 4.3 判別分析
mydata <- subset(koeppen, select=-c(t_year,p_year)) 
items <- c("A", "B", "C", "D")
abcd <- subset(mydata, mydata$class %in% items) # データ数が少ないEとHは除く。
levels(abcd$class) <- c("A", "B", "C", "D", "A", "B") # 因子の水準属性を変更。つまりEとHを除く。
#pairs(subset(mydata, select=-class), col=rainbow(4)[mydata$class])
## データをランダムに２分割 (一方を訓練データ, もう一方を試験データ)
set.seed(123)
idx <- sample.int(nrow(abcd), size=nrow(abcd)/2)
abcd.train <- abcd[idx, ] # 訓練データ
abcd.test <- abcd[-idx, ] # テストデータ
require(caret) 
## LOO CV の例 (lda/qdaは標準で装備している)
## 線形判別（データ数が少ないので上手くいかない）
myld <- lda(class ~ ., data=abcd.train)
myldloo <- lda(class ~ ., data=abcd.train, CV=TRUE)
table(true=abcd.train$class,est=predict(myld)$class) # 訓練誤差
table(true=abcd.train$class,est=myldloo$class)  # CVによる予測誤差
## 線形判別は過学習している
## 2次判別（データ数が少ないのでそもそも出来ない）
#myqd <- qda(class ~ ., data=abcd.train)
#myqdloo <- qda(class ~ ., data=abcd.train, CV=TRUE)
#table(true=abcd.train$class,est=predict(myqd)$class) # 訓練誤差
#table(true=abcd.train$class,est=myqdloo$class)  # CVによる予測誤差
