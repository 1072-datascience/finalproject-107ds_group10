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
#Parameters
nfolds <- 10
QuanNum <- 10
QuanTarget <- c('HEIGHT','WEIGHT','BUDGET')
QuanColName <- paste(QuanTarget,'_QUAN',sep ='')

#True Predict Test Data
{
  dTeBuy <- read.table('../data/test_buy_x_info.csv',header = TRUE, sep = ',')
  dTeCust <- read.table('../data/test_cust_x_info.csv',header = TRUE, sep = ',')
  dTeTpy <- read.table('../data/test_Tpy_x_info.csv',header = TRUE, sep = ',')
  
  
  dTeBuy <- SortCol(dTeBuy,'CUST_ID')
  dTeCust <- SortCol(dTeCust,'CUST_ID')
  dTeTpy <- SortCol(dTeTpy,'CUST_ID')
  
  dTeCust$CUST_ID <- NULL
  dTeTpy$CUST_ID <- NULL
  # Merge By CUST_ID
  dTeAll <- cbind(dTeBuy,dTeCust,dTeTpy)
  rm(dTeBuy,dTeCust,dTeTpy)
  dTeAll$BUY_YEAR <- NULL;
  dTeAll$BUY_MONTH <- factor(dTeAll$BUY_MONTH)
  
  
  
  Vars <- colnames(dTrAll[,-c(1,2)])
  Var_Cat <- Vars[sapply(dTrAll[,Vars],class) %in% c('factor','character')]
  Var_Cat <- c(Var_Cat,QuanColName)
  Var_Num <- Vars[sapply(dTrAll[,Vars],class) %in% c('numeric','integer')]
  Var_Num <- setdiff(Var_Num,QuanTarget)
  rm(Vars)
}
dTrAll <- ShuffleData(dTrAll)

FoldsIndicator <- BalancedSplitIndicator(Data = dTrAll, LabelCol = dTrAll$BUY_TYPE, nfolds = nfolds)
AccList <- list()
library('rlist');library('dataPreparation');library(e1071);library(DMwR)

DataProcess <- function(Data,IsTrain = FALSE, IsPred = FALSE, OHEModel = NULL, OHETarget = NULL,
                        DoQuantile = TRUE, QuantileNum = 10, QuanModel = NULL, QuanTarget = NULL){
  if(IsTrain){
    QuanModel <- MultiBuildQuantile(Data = Data,ToBuild = QuanTarget, QuantileNum = QuantileNum)
    output <- ApplyQuantile(Data,QuanModel, Remove = T)
    output$OCCUPATION <- SubString(output$OCCUPATION,Index = 1)
    output <- ReplaceNA(output)
    OHEModel <- BuildOHEmodel(output, OHETarget)
    for(i in 1:length(OHEModel)){
      OHEModel[[i]][[1]] <- OHEModel[[i]][[1]][-c(1)]
      OHEModel[[i]][[2]] <- OHEModel[[i]][[2]][-c(1)]
    }
    output <- ApplyOHE(output, OHEModel, Drop = TRUE, verb = F)
    output$BUY_TYPE <- BuyToNum(output$BUY_TYPE)
    output$CUST_ID <- NULL
    return(list(Data = output,QUAN = QuanModel,OHE = OHEModel))
  }else if (IsPred){
    output <- ApplyQuantile(Data,QuanModel, Remove = T)
    output$OCCUPATION <- SubString(output$OCCUPATION,Index = 1)
    output <- ReplaceNA(output)
    output <- ApplyOHE(output, OHEModel, Drop = TRUE, verb = F)
    output$CUST_ID <- NULL
    return(output)
  }else{
    output <- ApplyQuantile(Data,QuanModel, Remove = T)
    output$OCCUPATION <- SubString(output$OCCUPATION,Index = 1)
    output <- ReplaceNA(output)
    output <- ApplyOHE(output, OHEModel, Drop = TRUE, verb = F)
    output$BUY_TYPE <- BuyToNum(output$BUY_TYPE)
    output$CUST_ID <- NULL
    return(output)
  }
}

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
# BestVal <- 0
# BestModel <- NULL

# for (i in 1:nfolds){
  i <- 1
  print(paste(i,'fold:'))
  IndexTest <- i
  IndexVal <- ifelse(i+1 <= nfolds, i+1, i-9)
  dTest <- SplitByIndicator(Data = dTrAll,Indicator = FoldsIndicator, Target = IndexTest, exclude = F)
  dVal <- SplitByIndicator(Data = dTrAll,Indicator = FoldsIndicator, Target = IndexVal, exclude = F)
  dTrain <- SplitByIndicator(Data = dTrAll,Indicator = FoldsIndicator, Target = c(IndexTest, IndexVal), exclude = T)
  
  TMP <- DataProcess(dTrain,IsTrain = TRUE, OHETarget =Var_Cat, QuantileNum = 10, QuanTarget = QuanTarget)
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
  print('Training:')
  model <- naiveBayes(formula = BUY_TYPE ~ . , data = dTrain)
  print('Predicting Train:')
  TrainPreds <- predict(model,dTrain)
  print('Predicting Val:')
  ValPreds <- predict(model, dVal)
  print('Predicting Test:')
  TestPreds <- predict(model, dTest)
  
  Val.Acc <- Acc(ValPreds,dVal$BUY_TYPE)
  Test.Acc <- Acc(TestPreds,dTest$BUY_TYPE)
  Train.Acc <- Acc(TrainPreds,dTrain$BUY_TYPE)
  # if(Val.Acc > BestVal){
  #   BestModel <- model
  #   BestVal <- Val.Acc
  #   BestOHEModel <- OHEModel
  #   BestQuanModel <- QuanModel
  # }
  #AccList <- list.append(.data = AccList,c(Val.Acc,Test.Acc,Train.Acc))
  
#}

dPred <- DataProcess(dTeAll, IsPred = TRUE, OHEModel = OHEModel, QuanModel = QuanModel)
dPred <- ColRemove(dPred,Col_Double)
colnames(dPred) <- gsub(" ", "", colnames(dPred))

Pred <- predict(model,dPred)


Answer <- read.table('../data/[Answer] test_buy_y_info.csv',header = TRUE, sep = ',')
Answer <- SortCol(Answer,'CUST_ID')


Acc(Pred,Answer$BUY_TYPE)
Prediction <- data.frame(CUST_ID = dTeAll$CUST_ID, BUY_TYPE = Pred)
write.csv(Prediction, file = "../results/Bayes/prediction.csv", row.names = FALSE)
#AccuracyTable <- data.frame(t(data.frame(AccList)),row.names = NULL)
#colnames(AccuracyTable) <- c('Validation','Test','Train')
AccuracyTable <- data.frame(Train = round(Train.Acc, digits = 3), Val = round(Val.Acc, digits = 3), Test = round(Test.Acc, digits = 3)  ,row.names = NULL)
#AccuracyTable <- data.frame(sapply(AccuracyTable,function(x) round(x, digits = 3)))
write.csv(AccuracyTable,file = '../results/Bayes/Accuracy.csv',row.names = FALSE, quote = FALSE)


#save Trained model and related data
save(list = c('DataProcess','BuyToNum','NumToBuy','SortCol','SubString','ReplaceNA',
              'getmode','ApplyQuantile','ApplyOHE','Acc','Answer','Pred','ColRemove',
              'Col_Double'),
     file = '../results/Bayes/BayesModel.RData')

