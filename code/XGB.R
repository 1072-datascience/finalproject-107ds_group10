#XGB tree
cat('XGB training!\n')

QuanNum <- 10
QuanTarget <- c('HEIGHT','WEIGHT','BUDGET')
QuanColName <- paste(QuanTarget,'_QUAN',sep ='')

Vars <- colnames(dTrAll[,-c(1,2)])
Var_Cat <- Vars[sapply(dTrAll[,Vars],class) %in% c('factor','character')]
Var_Cat <- c(Var_Cat,QuanColName)
Var_Num <- Vars[sapply(dTrAll[,Vars],class) %in% c('numeric','integer')]
Var_Num <- setdiff(Var_Num,QuanTarget)
rm(Vars)

DataXGB <- dTrAll
DataXGB <- ShuffleData(DataXGB)

FoldsIndicator <- BalancedSplitIndicator(Data = DataXGB, LabelCol = DataXGB$BUY_TYPE, nfolds = Args_nfolds)
AccList <- list()
library('rlist');library('xgboost');library('dataPreparation');library('caret')

BestVal <- 0
BestModel <- NULL

for (i in 1:Args_nfolds){

  print(paste(i,'fold:'))
  IndexTest <- i
  IndexVal <- ifelse((i+1) <= Args_nfolds, i+1, i+1 - Args_nfolds)
  dTest <- SplitByIndicator(Data = DataXGB,Indicator = FoldsIndicator, Target = IndexTest, exclude = F)
  dVal <- SplitByIndicator(Data = DataXGB,Indicator = FoldsIndicator, Target = IndexVal, exclude = F)
  dTrain <- SplitByIndicator(Data =DataXGB,Indicator = FoldsIndicator, Target = c(IndexTest, IndexVal), exclude = T)
  
  TMP <- DataProcess(dTrain,IsTrain = TRUE, OHETarget =Var_Cat, QuantileNum = 10, QuanTarget = QuanTarget)
  dTrain <- TMP$Data
  OHEModel <- TMP$OHE
  QuanModel <- TMP$QUAN
  
  dVal <- DataProcess(dVal, OHEModel = OHEModel, QuanModel = QuanModel)
  dTest <- DataProcess(dTest, OHEModel = OHEModel, QuanModel = QuanModel)
  
  dtrain <- xgb.DMatrix(as.matrix( dTrain[,-c(1)]),label=dTrain$BUY_TYPE)
  dval <- xgb.DMatrix(as.matrix( dVal[,-c(1)]),label=dVal$BUY_TYPE)
  dtest <- xgb.DMatrix(as.matrix( dTest[,-c(1)]),label=dTest$BUY_TYPE)
  
  #param <- list(max_depth = 6, eta = 0.3, nthread = 10, objective = "multi:softmax" ,num_class = 7)
  #bst.mdl <- xgb.train(param, data = dtrain, nrounds = 10,verbose = 2)
  bst.mdl <- xgboost( data = dtrain, max_depth = 6, eta = 1, nthread = 10, objective = "multi:softmax" ,num_class = 7, nrounds = 10)
  
  TrainPreds <- predict(bst.mdl,dtrain)
  ValPreds <- predict(bst.mdl, dval)
  TestPreds <- predict(bst.mdl, dtest)
  
  TrainCM <- confusionMatrix(NumToBuy(TrainPreds),NumToBuy(dTrain$BUY_TYPE))
  ValCM <- confusionMatrix(NumToBuy(ValPreds),NumToBuy(dVal$BUY_TYPE))
  TestCM <- confusionMatrix(NumToBuy(TestPreds),NumToBuy(dTest$BUY_TYPE))
  
  
  Train.Acc <- TrainCM[[3]][1]
  Val.Acc <- ValCM[[3]][1]
  Test.Acc <- TestCM[[3]][1]
  if(Val.Acc > BestVal){
    BestModel <- bst.mdl
    BestVal <- Val.Acc
    BestOHEModel <- OHEModel
    BestQuanModel <- QuanModel
  }
  AccList <- list.append(.data = AccList,c(Val.Acc,Test.Acc,Train.Acc))
  rm(IndexTest, IndexVal, dTest, dVal, dTrain, TMP, OHEModel, QuanModel,
     dtrain, dval, dtest, bst.mdl, TrainPreds, ValPreds, 
     TestPreds, TrainCM, ValCM, TestCM, Train.Acc, Val.Acc, Test.Acc)
  
}

dPred <- DataProcess(dTeAll, IsPred = TRUE, OHEModel = BestOHEModel, QuanModel = BestQuanModel)
dpred <- xgb.DMatrix(as.matrix(dPred))
Pred <- NumToBuy(predict(BestModel,dpred))

confusionMatrix(Pred,Answer$BUY_TYPE)
#Acc(Pred,Answer$BUY_TYPE)
Prediction <- data.frame(CUST_ID = dTeAll$CUST_ID, BUY_TYPE = Pred)


if (dir.exists(paste(Args_output,'XGB/',sep = '')) == FALSE )
  dir.create(paste(Args_output,'XGB/',sep=''))
write.csv(Prediction, file = paste(Args_output,'XGB/prediction.csv',sep = '') , row.names = FALSE, quote = FALSE)

CM <- confusionMatrix(Prediction$BUY_TYPE,Answer$BUY_TYPE)

CSVOverall <- round(data.frame(CM$overall),digits = 4)
CSVByClass <- round(data.frame(CM$byClass),digits = 4)

# You can then use
write.csv(CSVOverall,file= paste(Args_output,'XGB/Overall.csv', sep = '') )
write.csv(CSVByClass,file= paste(Args_output,'XGB/ByClass.csv', sep = '') )


mat <- xgb.importance (feature_names = colnames(dPred),model = BestModel)
mat$GainOrder <- factor(mat$Feature, levels = mat$Feature[order(mat$Gain)])
mat$Cluster <- factor(seq(1:nrow(mat)) %/% 4 +1)
ggplot(mat[1:25],aes(x = GainOrder, y = Gain, fill = Cluster)) + geom_bar(stat = "identity") + coord_flip() 
ggsave( paste(Args_output,'XGB/GainTop25.png', sep = ''), device = 'png' ,width = 250, height = 200, units = "mm")

ggplot(mat,aes(x = GainOrder, y = Gain, fill = Cluster)) + geom_bar(stat = "identity") + coord_flip() 
ggsave( paste(Args_output,'XGB/Gain.png', sep = ''), device = 'png' , units = "mm")


rm('CM','CSVByClass','CSVOverall')

library('beepr');beep()
