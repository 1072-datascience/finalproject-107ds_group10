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

PlotOrNot <- FALSE
# load packages
library('ggplot2');library('ggthemes')
library('gridExtra')
library('dataPreparation')

#Map target into number by ascii
dTrAll$BUY <- as.vector(sapply(as.character(dTrAll$BUY_TYPE),utf8ToInt)) -97

#Train Test Val Split
#Shuffle data
dTrAll <- dTrAll[sample(nrow(dTrAll)),]

dTrAll$folds <- 0
nfolds <- 10
for (y in unique(dTrAll$BUY)){
  dTrAll$folds[dTrAll$BUY  == y] <-  cut(seq(1,sum(dTrAll$BUY == y)),breaks=nfolds,labels=FALSE)
}
rm(y)

#TODO: for loop to CV
library(data.table)
i <- 1
#Use 10th fold to test, 1~9th folds to train/val
Test.Indexes <- which((dTrAll$folds) == nfolds , arr.ind=TRUE)
Val.Indexes <-  which(dTrAll$folds== (i %% (nfolds-1)) ,arr.ind=TRUE)

dTrain <- dTrAll[-c(Test.Indexes,Val.Indexes),]
dVal <- dTrAll[Val.Indexes,]

#NA Dealing
dTrainFix <- copy(dTrain)

# OCCUPATION filter
# keep only the first letter
dTrainFix$OCCUPATION <- sapply(as.character(dTrain$OCCUPATION), function(x) strsplit(x, NULL)[[1]][1])
dTrainFix$OCCUPATION[dTrainFix$OCCUPATION == 'N' ] <- 'NA'

#Continuous Numerical column Quantilization
QUANTILE_num <- 10
#HEIGHT
HEIGHT_QUANTILE <- quantile(dTrainFix$HEIGHT,
                            probs = seq(0,1,length.out = QUANTILE_num + 1),
                            include.lowest = T, ordered_result = T,na.rm = T)
HEIGHT_QUANTILE[1] <- -Inf; HEIGHT_QUANTILE[length(HEIGHT_QUANTILE)] <- +Inf
dTrainFix$HEIGHT_QUAN <- as.ordered(cut(dTrainFix$HEIGHT,HEIGHT_QUANTILE))

#WEIGHT
WEIGHT_QUANTILE <- quantile(dTrainFix$WEIGHT,
                            probs = seq(0,1,length.out = QUANTILE_num + 1),
                            include.lowest = T, ordered_result = T,na.rm = T)
WEIGHT_QUANTILE[1] <- -Inf; WEIGHT_QUANTILE[length(WEIGHT_QUANTILE)] <- +Inf
dTrainFix$WEIGHT_QUAN <- as.ordered(cut(dTrainFix$WEIGHT,WEIGHT_QUANTILE))

#Budget Quantilization
BUDGET_QUANTILE <- quantile(dTrainFix$BUDGET,
                            probs = seq(0,1,length.out = QUANTILE_num + 1)
                            ,include.lowest = T , ordered_result = T)
BUDGET_QUANTILE[1] <- -Inf; BUDGET_QUANTILE[length(BUDGET_QUANTILE)] <- +Inf
dTrainFix$BUDGET_QUAN <- as.ordered(cut(dTrainFix$BUDGET,BUDGET_QUANTILE))

#Plot Edited Var
VarToPlot <- c('BUDGET_QUAN','HEIGHT_QUAN','WEIGHT_QUAN','OCCUPATION')
if(PlotOrNot){
pdf('img/data/OHE-After/HEIGHTnWEIGHT.pdf')
print(ggplot(dTrainFix, aes(x=HEIGHT_QUAN ,y=WEIGHT_QUAN,col=factor(BUY_TYPE))) + 
        geom_count() +
        facet_wrap(~factor(BUY_TYPE)) + theme_few() +
        theme(axis.text.x = element_text(size=5,angle=90)))
dev.off()
pdf('img/data/OHE-After/OCCUPATION.pdf')
print(ggplot(dTrainFix, aes(x=OCCUPATION)) 
      + geom_bar(stat='count') 
      + labs(x = 'OCCUPATION') 
      + theme_few()
      + ggtitle('Full- OCCUPATION'))
dev.off()
pdf('img/data/OHE-After/NewVar.pdf')
for(c in VarToPlot){
  print(paste('Plotting',c))
  PlotByGroup(dTrainFix, x = c, Group = 'BUY_TYPE',
              Relative = F, Title = paste(c,'\n-Groupby BUY_TYPE,\n Fixed Y'))
  PlotByGroup(dTrainFix, x = c, Group = 'BUY_TYPE',
              Relative = T, Title = paste(c,'\n-Groupby BUY_TYPE,\n Relative Y'))
  
  PlotByGroup(dTrainFix, x = 'BUY_TYPE', Group = c,
              Relative = F, Title = paste(c,'\n-Groupby ',c,',\n Fixed Y', sep ='') ) 
  PlotByGroup(dTrainFix, x = 'BUY_TYPE', Group = c,
              Relative = T, Title = paste(c,'\n-Groupby ',c,',\n Relative Y',sep=''))
}
dev.off()
}

#Replace NA with 'NA' in categorical columns
Var_Cat_NA <-  colnames(dTrainFix)[(sapply(dTrainFix,function(x) sum(is.na(x))) > 0)]
Var_Cat_NA <- Var_Cat_NA[-which(Var_Cat_NA == 'HEIGHT' | Var_Cat_NA == 'WEIGHT')]
for (c in Var_Cat_NA){
  dTrainFix[,c] <- `levels<-`(addNA(dTrainFix[,c]), c(levels(dTrainFix[,c]), 'NA'))
  #tmp2 <- factor(ifelse(is.na(tmp), 'NA', paste(tmp)), levels = c(levels(tmp), 'NA'))
}

#Dummy Var
Var_Cat <- c(Var_Cat,'BUDGET_QUAN','HEIGHT_QUAN','WEIGHT_QUAN')
OHEmodel <- build_encoding(dTrainFix, cols = Var_Cat, verbose = TRUE)
dTrainFix_OHE <- one_hot_encoder(dTrainFix, encoding = OHEmodel, drop = TRUE)

# location of given column names
CatLocation <- function(Data,ColNames){
  output <- c()
  for (c in ColNames){
    tmp <-  which(colnames(Data) == c )
    output <- c(output,tmp)
  }
  return(output)
}

# non-categorical column name
OHENotCat <- c("CUST_ID","BUY_TYPE","HEIGHT","WEIGHT","CHILD_NUM","BUDGET","BUY",'folds')
# categorical column after OHE
VarOHE_Cat <- colnames(dTrainFix_OHE)[-CatLocation(dTrainFix_OHE,OHENotCat)]

#plot the stat figure after OHE
if(PlotOrNot){
pdf('img/data/OHE-After/Categorical.pdf')
for (c in VarOHE_Cat){
  print(paste('Plotting:',c))
  #pdf(paste('img/data/CatOHE/',c,'.pdf',sep = "" ) )
  if (!(c %in% c('BUDGET_QUAN','HEIGHT_QUAN','WEIGHT_QUAN'))){
    print(ggplot(dTrainFix_OHE, aes_string(x=c,fill = 'factor(BUY_TYPE)')) 
          + geom_bar(stat='count',position = 'dodge') 
          + labs(x = c) 
          + theme_few()
          + scale_x_continuous(breaks =  sort(unique(dTrainFix_OHE[[as.integer(CatLocation(dTrainFix_OHE,c))]]))))
  }else{
    print(ggplot(dTrainFix_OHE, aes_string(x= c,fill = 'factor(BUY_TYPE)')) +
            geom_bar(stat='count', position='dodge') + labs(x = c ) +
            theme_few() +
            theme(axis.text.x = element_text(size=5,angle=90)))
  }
}
dev.off()
}

#remove unused var
rm(Var_Cat,Var_Num,Var_Cat_NA,VarOHE_Cat,OHENotCat,NA_Ratio)
save.image('02.RData')