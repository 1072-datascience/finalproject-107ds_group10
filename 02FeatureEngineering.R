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

# Backup Mean for Training and Predicting data
Mean_HEIGHT <- mean(dTrain$HEIGHT,na.rm = T)
Mean_WEIGHT <- mean(dTrain$WEIGHT,na.rm = T)

# Replace NA with mean(train) and truncate at +- Range
Threshold <- 5

tmp <- dTrain$HEIGHT
tmp[is.na(tmp)] <- Mean_HEIGHT
tmp[tmp > Threshold] <- Threshold
tmp[tmp < -Threshold] <- -Threshold
dTrainFix$HEIGHT <- tmp

tmp <- dTrain$WEIGHT
tmp[is.na(tmp)] <- Mean_WEIGHT
tmp[tmp > Threshold] <- Threshold
tmp[tmp < -Threshold] <- -Threshold
dTrainFix$WEIGHT <- tmp

rm(tmp,Threshold)

if(PlotOrNot){
# Plot HEIGHT/WEIGHT data Before/After fix
pdf('img/data/OHE-After/NAFix.pdf')
plotH_B <- ggplot(dTrain[!is.na(dTrain$HEIGHT),],aes(x = HEIGHT)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666",adjust = 2) + labs(x = 'HEIGHT-Before')
plotH_A <- ggplot(dTrainFix,aes(x = HEIGHT)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666",adjust = 2) + labs(x = 'HEIGHT-After')
grid.arrange(plotH_B, plotH_A, nrow=2, ncol=1)

rm(plotH_B,plotH_A)

plotW_B <- ggplot(dTrain[!is.na(dTrain$WEIGHT),],aes(x = WEIGHT)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666",adjust = 2) + labs(x = 'WEIGHT-Before')
plotW_A <- ggplot(dTrainFix,aes(x = WEIGHT)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666",adjust = 2) + labs(x = 'WEIGHT-After')
grid.arrange(plotW_B, plotW_A, nrow=2, ncol=1)

rm(plotW_B,plotW_A)
dev.off()
}

# OCCUPATION filter
# keep only the first letter
dTrainFix$OCCUPATION <- sapply(as.character(dTrain$OCCUPATION), function(x) strsplit(x, NULL)[[1]][1])
if(PlotOrNot){
pdf('img/data/OHE-After/OCCUPATION.pdf')
ggplot(dTrainFix, aes(x=OCCUPATION ,fill = factor(BUY_TYPE))) +
  geom_bar(stat='count', position='dodge') + labs(x = 'OCCUPATION' ) +
  facet_wrap(~factor(BUY_TYPE)) + theme_few()
dev.off()
}
#Budget Quantilization
BUDGET_QUANTILE <- quantile(dTrainFix$BUDGET,probs = seq(0,1,length.out=11),include.lowest = T , ordered_result = T)
BUDGET_QUANTILE[1] <- -Inf; BUDGET_QUANTILE[length(BUDGET_QUANTILE)] <- +Inf
dTrainFix$BUDGET_QUAN <- as.ordered(cut(dTrainFix$BUDGET,BUDGET_QUANTILE))

if(PlotOrNot){
pdf('img/data/OHE-After/BUDGET.pdf')
print(ggplot(dTrainFix, aes(x=BUDGET_QUAN ,fill = factor(BUY_TYPE))) +
  geom_bar(stat='count', position='dodge') + labs(x = 'BUDGET_QUAN' ) +
  facet_wrap(~factor(BUY_TYPE),scales='fixed') + theme_few() +
  theme(axis.text.x = element_text(size=5,angle=90)) +  ggtitle('Fixed Y') )
  #scale_x_discrete(labels = 1:length(levels(dTrainFix$BUDGET_QUAN)))

print(ggplot(dTrainFix, aes(x=BUDGET_QUAN ,fill = factor(BUY_TYPE))) +
  geom_bar(stat='count', position='dodge') + labs(x = 'BUDGET_QUAN' ) +
  facet_wrap(~factor(BUY_TYPE),scales='free_y') + theme_few() +
  theme(axis.text.x = element_text(size=5,angle=90)) +  ggtitle('Relative Y') )
  #scale_x_discrete(labels = 1:length(levels(dTrainFix$BUDGET_QUAN)))
dev.off()
}

#Replace NA with 'NA' in catagorical columns
Var_Cat_NA <-  colnames(dTrainFix)[(sapply(dTrainFix,function(x) sum(is.na(x))) > 0)]
for (c in Var_Cat_NA){
  dTrainFix[,c] <- `levels<-`(addNA(dTrainFix[,c]), c(levels(dTrainFix[,c]), 'NA'))
  #tmp2 <- factor(ifelse(is.na(tmp), 'NA', paste(tmp)), levels = c(levels(tmp), 'NA'))
}

#Dummy Var
Var_Cat <- c(Var_Cat,'BUDGET_QUAN')
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

# non-catagorical column name
OHENotCat <- c("CUST_ID","BUY_TYPE","HEIGHT","WEIGHT","CHILD_NUM","BUDGET","BUY",'folds')
# catagorical column after OHE
VarOHE_Cat <- colnames(dTrainFix_OHE)[-CatLocation(dTrainFix_OHE,OHENotCat)]

#plot the stat figure after OHE
if(PlotOrNot){
pdf('img/data/OHE-After/Catagorical.pdf')
for (c in VarOHE_Cat){
  print(paste('Plotting:',c))
  #pdf(paste('img/data/CatOHE/',c,'.pdf',sep = "" ) )
  if (c != 'BUDGET_QUAN'){
    print(ggplot(dTrainFix_OHE, aes_string(x=c,fill = 'factor(BUY_TYPE)')) 
          + geom_bar(stat='count',position = 'dodge') 
          + labs(x = c) 
          + theme_few()
          + scale_x_continuous(breaks =  sort(unique(dTrainFix_OHE[[as.integer(CatLocation(dTrainFix_OHE,c))]]))))
  }else{
    print(ggplot(dTrainFix_OHE, aes(x=BUDGET_QUAN ,fill = factor(BUY_TYPE))) +
            geom_bar(stat='count', position='dodge') + labs(x = 'BUDGET_QUAN' ) +
            theme_few() +
            theme(axis.text.x = element_text(size=5,angle=90)))
  }
}
dev.off()
}

#remove unused var
rm(Var_Cat,Var_Num,Var_Cat_NA,VarOHE_Cat,OHENotCat,NA_Ratio)
save.image('02.RData')