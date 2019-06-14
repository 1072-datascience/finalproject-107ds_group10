#Null model by Bayes
rm(list = ls());load('NB.RData');cat('\f')
library('e1071')
set.seed(9527)
#Parameters
QuanNum <- 10
QuanTarget <- c('HEIGHT','WEIGHT','BUDGET')
QuanColName <- paste(QuanTarget,'_QUAN',sep ='')
Var_Cat <- c(Var_Cat,QuanColName)

DataBayes <- ShuffleData(dTrAll)

ColRemove <- function(Data,Cols){
  Output <- Data 
  for (c in Cols){
    Output[,c] <- NULL
  }
  return(Output)
}


Col_Double <- c('BEHAVIOR.2.NA', 'STATUS2.NA', 'STATUS3.NA', 'STATUS4.NA',
                'IS.PHONE.NA', 'INTEREST2.NA', 'INTEREST3.NA', 'INTEREST4.NA', 
                'INTEREST5.NA', 'INTEREST6.NA', 'INTEREST7.NA', 'INTEREST8.NA', 
                'INTEREST9.NA', 'INTEREST10.NA', 'IS.APP.NA', 'WEIGHT.QUAN.NA')

#Train once only
FoldsIndicator <- BalancedSplitIndicator(Data = DataBayes, LabelCol = DataBayes$BUY_TYPE, nfolds = Args_nfolds)
i <- 1
IndexTest <- i
IndexVal <- ifelse(i + 1 <= Args_nfolds, i + 1, i - 9)
dTest <- SplitByIndicator(Data = DataBayes, Indicator = FoldsIndicator, Target = IndexTest, exclude = F)
dVal <- SplitByIndicator(Data = DataBayes, Indicator = FoldsIndicator, Target = IndexVal, exclude = F)
dTrain <- SplitByIndicator(Data = DataBayes, Indicator = FoldsIndicator, Target = c(IndexTest, IndexVal), exclude = T)

TMP <- DataProcess(dTrain, IsTrain = TRUE, OHETarget = Var_Cat, QuantileNum = 10, QuanTarget = QuanTarget)
dTrain <- TMP$Data
OHEModel <- TMP$OHE
QuanModel <- TMP$QUAN

dVal <- DataProcess(dVal, OHEModel = OHEModel, QuanModel = QuanModel)
dTest <- DataProcess(dTest, OHEModel = OHEModel, QuanModel = QuanModel)

dTrain <- ColRemove(dTrain,Col_Double)
dTest <- ColRemove(dTest,Col_Double)
dVal <- ColRemove(dVal,Col_Double)

colnames(dTrain) <- gsub(" ", "", colnames(dTrain))
colnames(dTest) <- gsub(" ", "", colnames(dTest))
colnames(dVal) <- gsub(" ", "", colnames(dVal))

dTrain$BUY_TYPE <- as.factor(NumToBuy(dTrain$BUY_TYPE))
dTest$BUY_TYPE <- as.factor(NumToBuy(dTest$BUY_TYPE))
dVal$BUY_TYPE <- as.factor(NumToBuy(dVal$BUY_TYPE))
print('Training naive Bayes:')
model <- naiveBayes(formula = BUY_TYPE ~ . , data = dTrain)
print('Predicting Train:')
TrainPreds <- predict(model,dTrain)
print('Predicting Val:')
ValPreds <- predict(model, dVal)
print('Predicting Test:')
TestPreds <- predict(model, dTest)

dPred <- DataProcess(dTeAll, IsPred = TRUE, OHEModel = OHEModel, QuanModel = QuanModel)
dPred <- ColRemove(dPred,Col_Double)
colnames(dPred) <- gsub(" ", "", colnames(dPred))

dPred$BUY_TYPE <- predict(model,dPred)

confusionMatrix(dPred$BUY_TYPE,Answer$BUY_TYPE)
print(ggplot(Answer, aes(x=BUY_TYPE,fill = factor(BUY_TYPE)))
      + geom_bar(stat='count', position='dodge')
      + labs(x = 'BUY_TYPE'))

print(ggplot(dPred, aes(x=BUY_TYPE,fill = factor(BUY_TYPE)))
      + geom_bar(stat='count', position='dodge')
      + labs(x = 'BUY_TYPE'))


# 
# 
# 
# cat('The accuracy of null model by getting the mode observation is:\n')
# cat(Acc(dPred,Answer,'BUY_TYPE'),'\n')
# 
# if (dir.exists(paste(Args_output,'Null/',sep = '')) == FALSE )
#   dir.create(paste(Args_output,'Null/',sep=''))
# write.csv(dPred, file = paste(Args_output,'Null/prediction.csv',sep = '') , row.names = FALSE, quote = FALSE)
# 
# CM <- confusionMatrix(dPred$BUY_TYPE,Answer$BUY_TYPE)
# 
# (CSVOverall <- data.frame(CM$overall))
# (CSVByClass <- data.frame(CM$byClass))
# 
# # You can then use
# write.csv(CSVOverall,file= paste(Args_output,'Null/Overall.csv', sep = '') )
# write.csv(CSVByClass,file= paste(Args_output,'Null/ByClass.csv', sep = '') )
