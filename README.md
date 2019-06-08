# 1072DS Final Project - Group 10

### Groups
| 姓名 | 系級 | 學號 |
|:-----:|:------:|:-----:|
|何冠廷|金碩一|107352011|
|王韋之|金碩一|107352012|
|焦祖傑|統計三|105304043|
|張智鈞|統計三|105304015|
|廖渝瑄|統計三|105304002|

### Goal

本專案目標是針對一客戶行為之資料集進行訓練，最終預測客戶之產品購買行為。

[//]: <> ()
[//]: <> (This is a comment.)
[//]: <> (A breif introduction about your project, i.e., what is your goal?)

### Demo 

+ 實際預測程式為 `code/Cathay.R`
+ 執行參數包含

| 項目 |     範例     |         說明        |
|------|--------------|---------------------|
|nfolds|       5      | k-fold CV fold 數   |
|train |data/training/| train data 所在位置 |
|test |  data/test/  | test  data 所在位置 |
|output|    results/  | 輸出位置            |

+ 執行需指定 train,test data 所在之路徑，預設分別放置於 data/training, data/test 資料夾下，指定路徑即可。
+ 輸出包含 null model 之預測、實際模型預測、交叉驗證各次之準確度、訓練完成之模型相關資料、預測後之評估(TPR, FPR ...等)
+ 執行範例
```R
Rscript code/Cathay.R --nfolds 5 --train data/training/ --test data/test/ --output results/
```

[//]: <> (You should provide an example commend to reproduce your result)
[//]: <> (```R)
[//]: <> (Rscript code/your_script.R --input data/training --output results/performance.tsv)
[//]: <> (```)
[//]: <> (* any on-line visualization)


## Folder organization and its related information

### docs

+ Presentation 資料夾下包含報告時用到之簡報以及相關報告。
+ Reference 資料夾下包含預測過程中用到之文獻。

[//]: <> (* Your presentation, 1072_datascience_FP_<yourID|groupName>.ppt/pptx/pdf, by **Jun. 25**)
[//]: <> (* Any related document for the final project)
[//]: <> (  * papers)
[//]: <> (  * software user guide)


### data

+ data/training/ 為訓練集資料
+ data/test/ 為測試集資料
+ Description/ 下包含官方對個特徵之說明、範例輸出以及測試集之解答

#### 預處理

+ 類別資料特徵者，將會轉為虛擬變數。
+ 近一步觀察資料分布發現，缺失值部分有有系統性的分佈，因此將 NA 視為特徵而非以眾數或平均數填入。
+ 由於數值資料大部分為標準化數據落入 [-5,5] 之間，但有少部分極端值也有相同特徵，因此將資料分為10個分量後轉為類別資料處理。

[//]: <> (* Source)
[//]: <> (* Input format)
[//]: <> (* Any preprocessing?)
[//]: <> (  * Handle missing data)
[//]: <> (  * Scale value)

### code

+ 分別採用取 Train 樣本之眾數以及 naive Bayes 作為 null model
+ 實際預測使用 xgboost 內的 softmax 方法做預測
+ 除了 null model 之外採用 k-fold 交叉驗證

[//]: <> (* Which method do you use?)
[//]: <> (* What is a null model for comparison?)
[//]: <> (* How do your perform evaluation? ie. Cross-validation, or extra separated data)




### results

+ 使用所有類別的 Accuracy 以及 RMSE 作為主要衡量標準，並且針對個別類別計算 TPR, FPR, TNR, FNR
+ 本預測困難的點在於 feature 大多為類別資料，若將特徵轉為虛擬變數，會使得特徵數量增大，增加訓練及預測困難度。

[//]: <> (* Which metric do you use )
[//]: <> (  * precision, recall, R-square)
[//]: <> (* Is your improvement significant?)
[//]: <> (* What is the challenge part of your project?)


## Reference

+ Code/implementation which you include/reference (__You should indicate in your presentation if you use code for others. Otherwise, cheating will result in 0 score for final project.__)
  + detachAllPackages
    + https://gist.github.com/genomewalker/4678be4d97aac3771ab11b2216c5764f
  + Visualization
    + https://ggplot2.tidyverse.org/reference/geom_bar.html
    + http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization
    + http://www.sthda.com/english/wiki/print.php?id=187
    + https://stackoverflow.com/questions/45822139/ggplot2-several-plots-per-single-facet
    + https://bookdown.org/rdpeng/RProgDA/customizing-ggplot2-plots.html
  + Data Preprocessing
    + https://cran.r-project.org/package=dataPreparation/dataPreparation.pdf
  + Training/Predicting
    + https://cran.r-project.org/web/packages/e1071/e1071.pdf
    + https://xgboost.readthedocs.io/en/latest/
+ Packages you use
  + Visualize Related
    + ggplot2
    + ggthemes
    + gridExtra
  + Training Related
    + data.table
    + dataPreparation
    + rlist
    + xgboost
    + e1071
    + DMwR
* Related publications