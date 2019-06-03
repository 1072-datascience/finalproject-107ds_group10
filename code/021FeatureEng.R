# unload all package for clean env
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()
# load image from last stage
rm(list=ls()) ; load('01.RData') ; cat('\f')

#Some Training parameters
#folds of CV
#nfolds <- 10
#Discretization
#number of quantiles for discretization
#QuanNum <- 10
#Target for discretization
#QuanTarget <- c('HEIGHT','WEIGHT','BUDGET')
#QuanColName <- paste(QuanTarget,'_QUAN',sep ='')
#OHE
#Vars <- colnames(dTrAll[,-c(1,2)])
#Var_Cat <- Vars[sapply(dTrAll[,Vars],class) %in% c('factor','character')]
#Var_Cat <- c(Var_Cat,QuanColName)
#Var_Num <- Vars[sapply(dTrAll[,Vars],class) %in% c('numeric','integer')]
#Var_Num <- setdiff(Var_Num,QuanTarget)

#Using ASCII to map a to 0, b to 1, etc...
BuyToNum <- function(DataCol){
  return (as.vector(sapply(as.character(DataCol),utf8ToInt)) -97)
}

#Inverse function for above
NumToBuy <- function(DataCol){
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

#remove unused var
rm(c, PlotOrNot, NA_Ratio,PlotByGroup, Var_Cat, Var_Num)
save.image('021.RData')