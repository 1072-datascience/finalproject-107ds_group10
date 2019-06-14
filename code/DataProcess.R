cat('Data Process!\n')

library('caret')

# remove constant column
dTrAll$BUY_YEAR <- NULL
# Transfer BUY_MONTH into factor
dTrAll$BUY_MONTH <- factor(dTrAll$BUY_MONTH)


Vars <- colnames(dTrAll[,-c(1,2)])
Var_Cat <- Vars[sapply(dTrAll[,Vars],class) %in% c('factor','character')]
Var_Num <- Vars[sapply(dTrAll[,Vars],class) %in% c('numeric','integer')]
rm(Vars)



#Transfer Y into number
#Using ASCII to map a to 0, b to 1, etc...
BuyToNum <- function(DataCol){
  return (as.vector(sapply(as.character(DataCol),utf8ToInt)) -97)
}
#Inverse function for above
NumToBuy <- function(DataCol,Factorized = T){
  if (Factorized)
    return (factor(as.vector(sapply(DataCol+97,intToUtf8))))
  else
    return (as.vector(sapply(DataCol+97,intToUtf8)))
}


BuildQuantile <- function(DataCol,QuantileNum = 10){
  Quantile <- quantile(DataCol,probs = seq(0,1,length.out = QuantileNum + 1),
                       include.lowest = T, ordered_result = T,na.rm = T)
  Quantile[1] <- -Inf
  Quantile[length(Quantile)] <- +Inf
  return(Quantile)
}

MultiBuildQuantile <- function(Data,ToBuild,QuantileNum = 10){
  library('rlist')
  #Quantile of Given columns
  output <- list()
  for (c in ToBuild){
    tmp <- BuildQuantile(Data[,c],QuantileNum)
    output <- list.append(output,tmp)
  }
  attr(output,'names') = ToBuild
  return (output)
}

ApplyQuantile <- function(Data,Model,Remove = F, ordered = F){
  for(c in attr(Model,'names') ){
    ColName <- paste(c,'_QUAN',sep = '')
    if (ordered){
      Data[,ColName] <- as.ordered(factor(cut(Data[,c],Model[[c]])))
    }else{
      Data[,ColName] <- factor(cut(Data[,c],Model[[c]]))
    }
    #Data[,ColName] <- as.ordered(cut(Data[,c],Model[[c]]))
  }
  if(Remove){
    for(c in attr(Model,'names')){
      Data[,c] <- NULL
    }
  }
  return (Data)
}



#Simple Split without dealing with different Y labels
SplitIndicator <- function(Data,nfolds){
  return (cut(seq(1,nrow(Data)),breaks=nfolds,labels=FALSE))
}

#Balanced Split Data
BalancedSplitIndicator <- function(Data,LabelCol,nfolds){
  output <- rep(0,nrow(Data))
  for (y in unique(LabelCol)){
    output[LabelCol == y] <- cut(seq(1,sum(LabelCol == y)),breaks=nfolds,labels=FALSE)
  }
  return (output)
}

ShuffleData <- function(Data){
  return (Data[sample(nrow(Data)),])
}


SplitByIndicator <- function(Data,Indicator,Target,exclude = F){
  Index <- c()
  for (t in Target){
    Index <- c(Index,which(Indicator == t))
  }
  if (exclude){
    output <- Data[-Index,]
  }else{
    output <- Data[Index,]
  }
  return (output)
}

#mathematical mode
getmode <- function(DataCol) {
  uniq <- unique(DataCol)
  return (uniq[which.max(tabulate(match(DataCol, uniq)))])
}

# substring
SubString <- function(DataCol,Index = 1){
  DataCol <- as.character(DataCol)
  DataCol[which(DataCol == 'NANA')] <- getmode(DataCol)
  DataCol <- sapply(DataCol, function(x) strsplit(x, NULL)[[1]][Index] )
  return(DataCol)
}

#Replace NA with "NA"
ReplaceNA <- function(Data,Exclude = NULL){
  Var_NA <-  colnames(Data)[(sapply(Data,function(x) sum(is.na(x))) > 0)]
  for (c in Exclude){
    Var_NA <- Var_NA[-which (Var_NA == c)]
  }
  for (c in Var_NA){
    Data[,c] <- `levels<-`(addNA(Data[,c]), c(levels(Data[,c]), 'NA'))
  }
  return(Data)
}

#Dummy Var model
BuildOHEmodel <- function(Data,Target,verb = TRUE){
  library('dataPreparation')
  return ( build_encoding(Data, cols = Target, verbose = verb) )
}

ApplyOHE <- function(Data, Model, Drop = TRUE,verb = TRUE){
  library('dataPreparation')
  return (one_hot_encoder(Data, encoding = Model, drop = Drop, verbose = verb))
}

# location of given column names
CatLocation <- function(Data,ColNames){
  output <- c()
  for (c in ColNames){
    tmp <-  which(colnames(Data) == c )
    output <- c(output,tmp)
  }
  return(output)
}
Acc <- function(PredData, ActualData, TargetColName){
  return (confusionMatrix(PredData[[TargetColName]],ActualData[[TargetColName]])[[3]][1])
}

DataProcess <- function(Data,IsTrain = FALSE, IsPred = FALSE, OHEModel = NULL, OHETarget = NULL,
                        DoQuantile = TRUE, QuantileNum = 10, QuanModel = NULL, QuanTarget = NULL){
  # remove constant column
  Data$BUY_YEAR <- NULL
  # Transfer BUY_MONTH into factor
  Data$BUY_MONTH <- factor(Data$BUY_MONTH)
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


source('code/Null.R')
#source('code/Bayes.R')
source('code/XGB.R')