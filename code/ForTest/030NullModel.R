# unload all package for clean env
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()
# load image from last stage
rm(list=ls()) ; load('021.RData') ; cat('\f')
Acc <- function(Pr,Tr){
  return(sum(Pr == Tr)/length(Tr))
}
SortCol <- function(DataFrame,ColName){
  return (DataFrame[order(DataFrame[ColName]),])
}
#True Predict Test Data
dPred <- read.table('../data/test_buy_x_info.csv',header = TRUE, sep = ',')
dPred <- SortCol(dPred,'CUST_ID')
dPred$PRED_BUY_TYPE <- getmode(dTrAll$BUY_TYPE)

Answer <- read.table('../data/[Answer] test_buy_y_info.csv',header = TRUE, sep = ',')
Answer <- SortCol(Answer,'CUST_ID')

Prediction <- data.frame(CUST_ID = dPred$CUST_ID, BUY_TYPE = dPred$PRED_BUY_TYPE)
write.csv(Prediction, file = "../results/Null/prediction.csv", row.names = FALSE)
print(paste('Accuracy:',Acc(dPred$PRED_BUY_TYPE,Answer$BUY_TYPE)))
