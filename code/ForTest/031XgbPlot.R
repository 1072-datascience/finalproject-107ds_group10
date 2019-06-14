library('xgboost');library('ggplot2');library('ggthemes');library('gridExtra')
load('021.RData')
load('../results/XGB_Tree/XGBModel.RData')

{dTeBuy <- read.table('../data/test/test_buy_x_info.csv',header = TRUE, sep = ',')
dTeCust <- read.table('../data/test/test_cust_x_info.csv',header = TRUE, sep = ',')
dTeTpy <- read.table('../data/test/test_Tpy_x_info.csv',header = TRUE, sep = ',')
SortCol <- function(DataFrame,ColName){
  return (DataFrame[order(DataFrame[ColName]),])
}
dTeBuy <- SortCol(dTeBuy,'CUST_ID')
dTeCust <- SortCol(dTeCust,'CUST_ID')
dTeTpy <- SortCol(dTeTpy,'CUST_ID')

dTeCust$CUST_ID <- NULL
dTeTpy$CUST_ID <- NULL
# Merge By CUST_ID
dTeAll <- cbind(dTeBuy,dTeCust,dTeTpy)
rm(dTeBuy,dTeCust,dTeTpy)
dTeAll$BUY_YEAR <- NULL;
dTeAll$BUY_MONTH <- factor(dTeAll$BUY_MONTH)}
dPred <- DataProcess(dTeAll, IsPred = TRUE, OHEModel = BestOHEModel, QuanModel = BestQuanModel)

Answer <- read.table('../data/Description/[Answer] test_buy_y_info.csv',header = TRUE, sep = ',')
Answer <- SortCol(Answer,'CUST_ID')
Ans <- Answer$BUY_TYPE

mat <- xgb.importance (feature_names = colnames(dPred),model = BestModel)
mat$GainOrder <- factor(mat$Feature, levels = mat$Feature[order(mat$Gain)])
mat$Cluster <- factor(seq(1:nrow(mat)) %/% 4 +1)
ggplot(mat[1:25],aes(x = GainOrder, y = Gain, fill = Cluster)) + geom_bar(stat = "identity") + coord_flip() 

ggsave("../results/XGB_Tree/GainTop25.png", device = 'png' ,width = 250, height = 200, units = "mm")


#xgb.plot.importance (importance_matrix = mat[1:20]) 
#xgb.plot.tree(model = BestModel, tree = 0)


library('caret')
CM <- confusionMatrix (as.factor(Pred) ,Answer$BUY_TYPE)
print(CM)